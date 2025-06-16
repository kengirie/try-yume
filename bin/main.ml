(* Nostr relay client to fetch posts from wss://yabu.me and maintain connection *)

(* Nostr relay URL *)
let relay_url = "wss://yabu.me"

(* Generate a random subscription ID *)
let generate_subscription_id () =
  let random_bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set random_bytes i (Char.chr (Random.int 256))
  done;
  Bytes.to_string random_bytes
  |> String.map (fun c ->
      let code = Char.code c in
      if (code >= 48 && code <= 57) || (code >= 97 && code <= 122) then c
      else Char.chr (97 + (code mod 26)))

(* Create a REQ message to subscribe to events *)
let create_req_message subscription_id =
  (* Simple JSON construction for the REQ message *)
  Printf.sprintf {|["REQ", "%s", {"kinds": [1], "limit": 10}]|} subscription_id

(* Check if a message is an EOSE message for our subscription *)
let is_eose_message message subscription_id =
  try
    (* More robust EOSE detection - check if the message contains the pattern *)
    let pattern = Printf.sprintf {|"EOSE","%s"|} subscription_id in
    let rec contains_substring s sub start =
      if start > String.length s - String.length sub then false
      else if String.sub s start (String.length sub) = sub then true
      else contains_substring s sub (start + 1)
    in
    contains_substring message pattern 0
  with _ -> false

(* Parse EVENT message to extract post content *)
let parse_event message =
  (* Basic string parsing to extract event content *)
  (* In a real implementation, you would use a proper JSON parser *)
  try
    (* Check if it's an EVENT message *)
    if String.length message > 10 && String.sub message 0 8 = {|["EVENT"|} then
      (* Find the content field in the JSON - this is a simplified approach *)
      let content_pattern = {|"content":"|} in
      let content_pos =
        try String.index_from message 0 (String.get content_pattern 0) with Not_found -> (-1) in

      if content_pos >= 0 then
        (* Try to find the content field *)
        let rec find_content_start pos =
          if pos + String.length content_pattern > String.length message then (-1)
          else if String.sub message pos (String.length content_pattern) = content_pattern then
            pos + String.length content_pattern
          else find_content_start (pos + 1)
        in

        let content_start = find_content_start content_pos in

        if content_start >= 0 then
          (* Find the closing quote of the content *)
          let rec find_content_end pos in_escape =
            if pos >= String.length message then (-1)
            else if message.[pos] = '"' && not in_escape then pos
            else if message.[pos] = '\\' then find_content_end (pos + 1) (not in_escape)
            else find_content_end (pos + 1) false
          in

          let content_end = find_content_end content_start false in

          if content_end >= 0 then
            Some (String.sub message content_start (content_end - content_start))
          else None
        else None
      else None
    else None
  with _ -> None

(* Main program *)
let () =
  (* Initialize random number generator *)
  Random.self_init ();
  Mirage_crypto_rng_unix.use_default ();

  (* Initialize Eio scheduler *)
  Eio_main.run @@ fun env ->

  (* Create switchboard *)
  Eio.Switch.run @@ fun sw ->

    Printf.printf "Connecting to Nostr relay: %s\n%!" relay_url;

    try
      (* Connect to WebSocket *)
      let conn = Yume.Ws.Client.connect ~sw env relay_url in

      Printf.printf "Connection successful! Connection ID: %s\n%!" (Yume.Ws.Client.id conn);

      (* Generate a subscription ID *)
      let subscription_id = generate_subscription_id () in
      Printf.printf "Subscription ID: %s\n%!" subscription_id;

      (* Create and send REQ message *)
      let req_message = create_req_message subscription_id in
      Printf.printf "Sending subscription request: %s\n%!" req_message;

      (* Create text frame and send it *)
      let frame = Websocket.Frame.create ~content:req_message () in
      Yume.Ws.Client.write conn frame;

      (* Collect posts indefinitely *)
      Printf.printf "Waiting for posts... (Press Ctrl+C to exit)\n%!";

      let posts = ref [] in
      let count = ref 0 in
      let eose_received = ref false in

      (* Read messages indefinitely *)
      (try
        while true do
          (* Read a frame from the server *)
          let response = Yume.Ws.Client.read conn in

          match response.Websocket.Frame.opcode with
          | Websocket.Frame.Opcode.Text ->
              let message = response.Websocket.Frame.content in

              (* Debug: Print the raw message *)
              Printf.printf "Raw message: %s\n%!" message;

              (* Check if it's an EOSE message *)
              if is_eose_message message subscription_id then begin
                if not !eose_received then begin
                  Printf.printf "Received EOSE message. All stored events have been sent. Continuing to listen for new events...\n%!";
                  eose_received := true;

                  (* Display all collected posts so far *)
                  Printf.printf "\nCollected %d posts so far:\n%!" (List.length !posts);
                  List.iteri (fun i post ->
                    Printf.printf "Post %d: %s\n%!" (i + 1) post
                  ) (List.rev !posts);
                  Printf.printf "\nContinuing to listen for new events... (Press Ctrl+C to exit)\n%!";
                end else begin
                  Printf.printf "Received another EOSE message.\n%!";
                end
              end else begin
                (* Try to parse the event *)
                match parse_event message with
                | Some content ->
                    incr count;
                    posts := content :: !posts;
                    Printf.printf "Received post %d: %s\n%!" !count content
                | None ->
                    Printf.printf "Received non-event message\n%!"
              end
          | Websocket.Frame.Opcode.Close ->
              Printf.printf "Connection closed by server\n%!";
              raise Exit
          | _ ->
              Printf.printf "Received other type of message\n%!";
        done
      with
      | Exit -> ()
      | Sys.Break ->
          Printf.printf "\nReceived interrupt signal. Closing connection...\n%!");

      (* Send CLOSE message to end subscription *)
      let close_message = Printf.sprintf {|["CLOSE", "%s"]|} subscription_id in
      Printf.printf "Sending close message: %s\n%!" close_message;

      (* Create text frame and send it *)
      let close_frame = Websocket.Frame.create ~content:close_message () in
      (try Yume.Ws.Client.write conn close_frame with _ -> ());

      (* Display all collected posts *)
      Printf.printf "\nCollected %d posts in total:\n%!" (List.length !posts);
      List.iteri (fun i post ->
        Printf.printf "Post %d: %s\n%!" (i + 1) post
      ) (List.rev !posts);

      (* Let the connection close naturally by exiting the switch *)
      Printf.printf "Program completed successfully\n%!"

    with e ->
      Printf.printf "Error occurred: %s\n%!" (Printexc.to_string e)
