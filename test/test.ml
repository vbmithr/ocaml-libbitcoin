open Sodium
open Libbitcoin

let gen_32bytes () =
  Sodium.Random.Bytes.generate 32

let test_transaction () =
  let open Transaction in
  let sk = Ec_private.of_wif_exn "cQLub6EDzTRhBxESxZPJ7XAUcwnM8XyTgNM4sr1S8AzxBp2LQpPc" in
  let pk = Ec_public.of_private sk in
  let secret = Ec_private.secret sk in
  let txid = `Hex "80720308cf2830cfe79bcf28ca94d87c7bfd4040771688003ee22ffe7f974050" in
  let scriptPubKey = `Hex "76a9148f3a46528003916c72c0e8e0d40a9175d5d6b74088ac" in
  let tx = Hash.Hash32.of_hex_exn txid in
  let previous_output = Output_point.create ~tx ~txid:1 in
  let prev_out_script = match Script.of_hex scriptPubKey with
    | Some script -> script
    | None -> invalid_arg "prev_out_script" in
  assert (Script.is_valid prev_out_script) ;
  let input = Input.create ~previous_output ~script:(Script.invalid ()) () in
  assert (Input.is_valid input) ;
  let payment_addr = Payment_address.of_b58check_exn "3EAbU8GtLymWvcqebCZUwYuZV1QcHsxqzb" in
  let script = Payment_address.to_script payment_addr in
  assert (Script.is_valid script) ;
  let output = Output.create ~value:10_000L ~script in
  assert (Output.is_valid output) ;
  let tx = create [input] [output] in
  let Sign.Endorsement endorsement =
    match Sign.endorse ~tx ~input_id:0 ~prev_out_script ~secret () with
  | Some endorsement -> endorsement
  | None -> invalid_arg "Sign.endorse" in
  let script = Script.endorsement endorsement pk in
  assert (Script.is_valid script) ;
  let new_inputs =
    ListLabels.map (get_inputs tx) ~f:(fun i -> Input.set_script i script; i) in
  set_inputs tx new_inputs ;
  begin match Transaction.check tx with
    | Ok () -> ()
    | Error msg -> Printf.printf "Error: %s\n" msg ;
  end ;
  let `Hex tx_hex = Data.Chunk.to_hex (to_data tx) in
  let `Hex tx_hex_nowire = Data.Chunk.to_hex (to_data ~wire:false tx) in
  Printf.printf "%s\n" tx_hex ;
  Printf.printf "%s\n" tx_hex_nowire

let () =
  Sodium.Random.stir () ;
  let privkeys = ListLabels.map [() ; () ; ()] ~f:begin fun () ->
      let seed = gen_32bytes () in
      Ec_private.Ec_secret.of_bytes seed
    end in
  let privkeys =
    ListLabels.map privkeys ~f:Ec_private.(of_secret ~version:Testnet) in
  ListLabels.iter privkeys ~f:begin fun sk ->
    print_endline @@ Ec_private.encode sk
  end ;
  let pubkeys = ListLabels.map privkeys ~f:Ec_public.of_private in
  ListLabels.iter pubkeys ~f:begin fun pk ->
    let `Hex pk_hex = Ec_public.encode pk in
    print_endline pk_hex
  end ;
  let addrs = ListLabels.map pubkeys
      ~f:(Payment_address.of_point ~version:Testnet_P2KH) in
  let _script = Script.create_multisig ~threshold:2 pubkeys in
  let addrs_encoded = ListLabels.map addrs ~f:Payment_address.to_b58check in
  ListLabels.iter addrs_encoded ~f:begin fun addr_str ->
    match Payment_address.of_b58check addr_str with
    | Some _ -> print_endline addr_str
    | None -> raise Exit
  end ;
  test_transaction ()

