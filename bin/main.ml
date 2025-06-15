(*let () =  Mirage_crypto_rng_unix.use_default ();
 Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
        Yume.Client.get env ~sw "https://example.com"
        |> Yume.Client.Response.drain
        |> Eio.traceln "%s"
))*)

let websocket_url = "wss://ws.postman-echo.com/raw"

(* メインプログラム *)
let () =
  (* 乱数生成器を初期化 *)
  Mirage_crypto_rng_unix.use_default ();

  (* Eioスケジューラーを初期化 *)
  Eio_main.run @@ fun env ->

  (* スイッチボードを作成 *)
  Eio.Switch.run @@ fun sw ->

    Printf.printf "WebSocketサーバーに接続中: %s\n%!" websocket_url;

    try
      (* WebSocketに接続 *)
      let conn = Yume.Ws.Client.connect ~sw env websocket_url in

      Printf.printf "接続成功! 接続ID: %s\n%!" (Yume.Ws.Client.id conn);

      (* メッセージを送信 *)
      let message = "こんにちは、WebSocket!" in
      Printf.printf "メッセージを送信: %s\n%!" message;

      (* テキストフレームを作成して送信 *)
      let frame = Websocket.Frame.create ~content:message () in
      Yume.Ws.Client.write conn frame;

      (* 応答を待機 *)
      Printf.printf "応答を待機中...\n%!";

      (* サーバーからの応答を読み取る *)
      let response = Yume.Ws.Client.read conn in

      (* 応答を表示 *)
      (match response.Websocket.Frame.opcode with
      | Websocket.Frame.Opcode.Text ->
          Printf.printf "テキスト応答を受信: %s\n%!" response.Websocket.Frame.content
      | Websocket.Frame.Opcode.Binary ->
          Printf.printf "バイナリ応答を受信: %d バイト\n%!"
            (String.length response.Websocket.Frame.content)
      | Websocket.Frame.Opcode.Close ->
          Printf.printf "接続が閉じられました\n%!"
      | _ ->
          Printf.printf "その他の応答を受信\n%!");

      (* 接続を閉じる *)

      Yume.Ws.Client.close_transport conn;

      Printf.printf "WebSocket接続を閉じました\n%!"
    with e ->
      Printf.printf "エラーが発生しました: %s\n%!" (Printexc.to_string e)
