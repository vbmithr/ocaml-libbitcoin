open Sodium
open Libbitcoin
open OUnit2

let gen_32bytes () =
  Sodium.Random.Bytes.generate 32

let test_payment_address ctx =
  let addr_b58 = `Base58 "mjVrE2kfz42sLR5gFcfvG6PwbAjhpmsKnn" in
  let addr = Payment_address.of_b58check_exn addr_b58 in
  let addr_b58' = Payment_address.to_b58check addr in
  assert_equal addr_b58 addr_b58' ;
  let `Hex addr_hex = Payment_address.to_hex addr in
  assert_equal 40 (String.length addr_hex) ;
  let { Base58.Versioned.version ; payload } =
    Base58.Versioned.of_base58_exn addr_b58 in
  assert_equal ~printer:string_of_int 20 (String.length payload) ;
  assert_equal Base58.Versioned.Testnet_P2PKH version

let test_wif ctx =
  assert_equal None
    (Ec_private.of_wif "fc2bedd07294f44a5d42f716bb1329d23e8353a638058b2ba71bafccce7ae5f2") ;
  OUnit2.assert_bool "test_wif: valid wif key"
    (Ec_private.of_wif "cW2tb8iwYTfp6pRBAp7JcWsJfYtidtfhkFPhQLFrQGurR9iiNzGZ" <> None)

let test_mnemonic ctx =
  let entropy = Sodium.Random.Bytes.generate 20 in
  let mnemo = Mnemonic.of_entropy entropy in
  Printf.printf "%s\n" (String.concat " " mnemo) ;
  begin match Mnemonic.to_seed ~passphrase:"Bleh" mnemo with
  | None -> failwith "Mnemonic.to_seed"
  | Some seed ->
      let `Hex seed_hex = Hex.of_string seed in
      Printf.printf "%s\n" seed_hex ;
  end ;
  begin match Mnemonic.to_seed mnemo with
  | None -> failwith "Mnemonic.to_seed"
  | Some seed ->
      let `Hex seed_hex = Hex.of_string seed in
      Printf.printf "%s\n" seed_hex ;
  end

let test_transaction ctx =
  let open Transaction in

  (* Decode test_transaction *)
  let test_transaction =
    `Hex "01000000017b1eabe0209b1fe794124575ef807057c77ada2138ae4f\
          a8d6c4de0398a14f3f00000000494830450221008949f0cb400094ad\
          2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab893502207965\
          6090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff86\
          6a5f01ffffffff01f0ca052a010000001976a914cbc20a7664f2f69e\
          5355aa427045bc15e7c6c77288ac00000000" in
      begin
    match Transaction.of_hex test_transaction with
  | None -> invalid_arg "unable to decode transaction"
  | Some t ->
      Format.printf "%a@." Transaction.pp t
  end ;

  let sk = Ec_private.of_wif_exn
      "cQLub6EDzTRhBxESxZPJ7XAUcwnM8XyTgNM4sr1S8AzxBp2LQpPc" in
  let pk = Ec_public.of_private sk in
  let secret = Ec_private.secret sk in
  let txid =
    `Hex "80720308cf2830cfe79bcf28ca94d87c7bfd4040771688003ee22ffe7f974050" in
  let scriptPubKey =
    `Hex "76a9148f3a46528003916c72c0e8e0d40a9175d5d6b74088ac" in
  let tx = Hash.Hash32.of_hex_exn txid in
  let input =
    Input.create ~prev_out_hash:tx ~prev_out_index:1 ~script:(Script.invalid ()) () in
  assert (Input.is_valid input) ;
  let payment_addr =
    Payment_address.of_b58check_exn (`Base58 "3EAbU8GtLymWvcqebCZUwYuZV1QcHsxqzb") in
  let script = Payment_address.to_script payment_addr in
  assert (Script.is_valid script) ;
  let output = Output.create ~value:10_000L ~script in
  assert (Output.is_valid output) ;
  let tx = create [input] [output] in
  Format.printf "%a@." Transaction.pp tx ;
  let `Hex tx_hex = Transaction.to_hex tx in
  Printf.printf "%s\n" tx_hex ;
  let prev_out_script = match Script.of_hex scriptPubKey with
    | Some script -> script
    | None -> invalid_arg "prev_out_script" in
  assert (Script.is_valid prev_out_script) ;
  let endorsement =
    Sign.endorse_exn ~tx ~index:0 ~prev_out_script ~secret () in
  let scriptSig = Script.P2PKH.scriptSig endorsement pk in
  assert (Script.is_valid scriptSig) ;
  let new_inputs =
    ListLabels.map tx.inputs.inputs ~f:(fun i -> Input.set_script i script; i) in
  set_inputs tx new_inputs ;
  begin match Transaction.check tx with
    | Ok () -> ()
    | Error msg -> Printf.printf "tx: %s\n" msg ;
  end ;
  let `Hex tx_hex = to_hex tx in
  let `Hex tx_hex_nowire = to_hex ~wire:false tx in
  Printf.printf "%s\n" tx_hex ;
  Printf.printf "%s\n" tx_hex_nowire ;
  let tx_chunk_parsed =
    begin match Transaction.of_hex (`Hex tx_hex) with
      | Some tx -> tx
      | None -> failwith "Transaction.from_data"
    end
  in
  let tx_chunk_parsed_nowire =
    begin match Transaction.of_hex ~wire:false (`Hex tx_hex_nowire) with
      | Some tx -> tx
      | None -> failwith "Transaction.from_data"
    end
  in
  Format.printf "%a@." Transaction.pp tx_chunk_parsed ;
  Format.printf "%a@." Transaction.pp tx_chunk_parsed_nowire ;
  begin match Transaction.check tx_chunk_parsed with
    | Ok () -> ()
    | Error msg -> Printf.printf "tx_chunk_parsed: %s\n" msg ;
  end ;
  assert (Transaction.is_valid tx_chunk_parsed_nowire) ;
  begin match Transaction.check tx_chunk_parsed_nowire with
    | Ok () -> ()
    | Error msg -> Printf.printf "tx_chunk_parsed_nowire: %s\n" msg ;
  end

let test_basic ctx =
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
      ~f:(Payment_address.of_point ~version:Testnet_P2PKH) in
  let _script = Script.P2SH_multisig.scriptRedeem ~threshold:2 pubkeys in
  let addrs_encoded = ListLabels.map addrs ~f:Payment_address.to_b58check in
  ListLabels.iter addrs_encoded ~f:begin fun ((`Base58 addr_str) as addr) ->
    match Payment_address.of_b58check addr with
    | Some _ -> print_endline addr_str
    | None -> raise Exit
  end

let suite =
  "libbitcoin" >::: [
    "test_basic" >:: test_basic ;
    "test_transaction" >:: test_transaction ;
    "test_mnemonic" >:: test_mnemonic ;
    "test_wif" >:: test_wif ;
    "test_payment_address" >:: test_payment_address ;
  ]

let () = run_test_tt_main suite
