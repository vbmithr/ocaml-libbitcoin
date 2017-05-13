open Sodium
open Libbitcoin
open OUnit2

let gen_32bytes () =
  Sodium.Random.Bytes.generate 32

let test_block ctx =
  let block = `Hex "030000009185dbc5e60723af6b4cdcdb5ceea505bc1cf7fe85097d02000000000000000001fef45c2701088577dffd37927b20da129acce6303f2ba3116ce032b4f8a5018de2dd55c4431418cb638acc0201000000010000000000000000000000000000000000000000000000000000000000000000ffffffff5703a7ab052f4249503130302f048fe2dd550850030f240c3900003c5b4254434368696e612e636f6d5d20e5b9b8e7a68fe4b88de59ca8e5be97e588b0e5a49a20e8808ce59ca8e8aea1e8be83e5b0912d2de4b99de6809d000000000100f90295000000001976a9142c30a6aaac6d96687291475d7d52f4b469f665a688ac000000000100000001f71c1cd429d1800080147ef63b2aa7440273d1ecdb2b0a1da01aded965e2ca8e000000006b483045022100de5bdb5a365fb16cc4057f1b1c1d9aabf130e85a9da6c184f186d0d0fbe7afd7022024321c4a53c4f5017153a666e10c65e4d790eb100fb4d66eaac8f9417699351c012102163e80de410646145142636833d8a92de4bb5c99e49bd52be5346fb1030628d4ffffffff02f05e3102000000001976a9145ca26d65ee83f441ef98b624763a305d50eb36cf88aca0860100000000001976a914838eb1034b719f9c47ab853aee63d505e4176a8388ac00000000" in
  let block = Block.of_bytes (Hex.to_string block) in
  assert_equal true (block <> None) ;
  match block with
  | None -> assert_bool "Block.of_bytes" false
  | Some block -> Format.printf "%a@." Block.pp block

let test_script ctx =
  let open Script in
  let mnemonic = "[aaaa] [bbbb]" in
  let mnemonic2 = "[3044022040aedf5483fde7b8fef90077a4c2c43fd348d6e378173e03bfd985f7c372b76a0220020a696b86bd7ab9211a92f17d8bb50e110a6fb07872237a2b05c6ea65ddf4db01] [02712321dd9dd72c285b3dd306445381e19d9b6461e94322ac0f5fdc6dc807395c]" in
  let script2 = of_mnemonic mnemonic2 in
  assert_equal true (script2 <> None) ;
  let script = of_mnemonic mnemonic in
  assert_equal true (script <> None) ;
  match script with
  | None -> assert_bool "Script.of_mnemonic" false
  | Some script -> begin
      let opcodes = Script.Opcode.[Data "\xaa\xaa" ; Data "\xbb\xbb"] in
      let script' = of_script opcodes in
      assert_equal ~printer:(fun x -> x) mnemonic (to_string script') ;
      let mnemonic' = to_string script in
      assert_equal ~printer:(fun x -> x) mnemonic mnemonic'
    end

let test_output_list ctx =
  let open Transaction in
  let script = Script.invalid () in
  let o1 = Output.create ~script ~value:1L in
  let o2 = Output.create ~script ~value:2L in
  let tx = Transaction.create [] [o1 ; o2] in
  Output.set_value o1 3L ;
  Printf.printf "%s\n" (Transaction.show tx) ;
  let `Hex tx_hex = Transaction.to_hex tx in
  Printf.printf "%s\n" tx_hex ;
  match Transaction.get_outputs tx with
  | [o1 ; o2] -> assert_equal 3L (Output.get_value o1)
  | _ -> failwith "test_output_list"

let test_payment_address ctx =
  let addr_b58 = Base58.Bitcoin.of_string_exn "mjVrE2kfz42sLR5gFcfvG6PwbAjhpmsKnn" in
  let addr = Payment_address.of_b58check_exn addr_b58 in
  let addr_b58' = Payment_address.to_b58check addr in
  assert_equal addr_b58 addr_b58' ;
  (* let `Hex addr_hex = Payment_address.to_hex addr in *)
  (* assert_equal 40 (String.length addr_hex) ; *)
  let { Base58.Bitcoin.version ; payload } = addr_b58 in
  assert_equal ~printer:string_of_int 20 (String.length payload) ;
  assert_equal Base58.Bitcoin.Testnet_P2PKH version

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
  assert_bool "input is valid" (Input.is_valid input) ;
  assert_bool "input size > 0" (Input.serialized_size input > 0) ;
  let input_bytes = Input.to_bytes input in
  begin match Input.of_bytes input_bytes with
  | None -> assert_bool "Input.of_bytes" false
  | Some input ->
    assert_bool "input is valid" (Input.is_valid input) ;
    let input_bytes' = Input.to_bytes input in
    assert_equal ~printer:(fun x -> x) input_bytes input_bytes'
  end ;
  let payment_addr =
    Payment_address.of_b58check_exn
      (Base58.Bitcoin.of_string_exn "3EAbU8GtLymWvcqebCZUwYuZV1QcHsxqzb") in
  let script = Payment_address.to_script payment_addr in
  assert_bool "Script.is_valid" (Script.is_valid script) ;
  let output = Output.create ~value:10_000L ~script in
  assert_bool "Output.is_valid" (Output.is_valid output) ;
  let tx = create [input] [output] in

  begin match Transaction.get_inputs tx with
  | [input'] ->
    assert_equal 0 (Transaction.Input.compare input input')
  | _ -> failwith "Transaction.get_inputs"
  end ;

  begin match Transaction.get_outputs tx with
  | [output'] ->
    assert_equal 0 (Transaction.Output.compare output output')
  | _ -> failwith "Transaction.get_outputs"
  end ;

  Transaction.Output.set_value output 10_001L ;
  begin match Transaction.get_outputs tx with
    | [o] ->
      assert_equal 10_001L (Transaction.Output.get_value o)
    | _ -> failwith "Transaction.get_outputs"
  end ;

  Format.printf "%a@." Transaction.pp tx ;
  let `Hex tx_hex = Transaction.to_hex tx in
  Printf.printf "%s\n" tx_hex ;
  let prev_out_script = match Script.of_hex scriptPubKey with
    | Some script -> script
    | None -> invalid_arg "prev_out_script" in
  assert_bool "Script.is_valid" (Script.is_valid prev_out_script) ;
  let endorsement =
    Sign.endorse_exn ~tx ~index:0 ~prev_out_script ~secret () in
  let scriptSig = Script.P2PKH.scriptSig endorsement pk in
  assert_bool "Script.is_valid" (Script.is_valid scriptSig) ;
  let new_inputs =
    ListLabels.map (get_inputs tx) ~f:(fun i -> Input.set_script i script; i) in
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
  assert_bool "Transaction.is_valid" (Transaction.is_valid tx_chunk_parsed_nowire) ;
  begin match Transaction.check tx_chunk_parsed_nowire with
    | Ok () -> ()
    | Error msg -> Printf.printf "tx_chunk_parsed_nowire: %s\n" msg ;
  end

let test_basic ctx =
  Sodium.Random.stir () ;
  let privkeys = ListLabels.map [() ; ()] ~f:begin fun () ->
      let seed = gen_32bytes () in
      Ec_private.Ec_secret.of_bytes seed
    end in
  let privkeys =
    ListLabels.map privkeys ~f:Ec_private.(of_secret ~testnet:true) in
  ListLabels.iter privkeys ~f:begin fun sk ->
    print_endline @@ Ec_private.encode sk
  end ;
  let pubkeys = ListLabels.map privkeys ~f:Ec_public.of_private in
  ListLabels.iter pubkeys ~f:begin fun pk ->
    let `Hex pk_hex = Ec_public.to_hex pk in
    print_endline pk_hex
  end ;
  let addrs = ListLabels.map pubkeys
      ~f:(Payment_address.of_point ~version:Testnet_P2PKH) in
  let scriptRedeem = Script.(P2SH_multisig.scriptRedeem ~threshold:2 pubkeys |> of_script) in
  let ed = `Hex "3045022100bbc77e7661e80533ed49ccbf0f3cf7ed3e573322f200de18041c8c1b06a22f4902200b4ed7b645365253b745dcea5218b2a8e8cdd237a6fb16b8ce6567e6dc1f126c01" in
  let ed = Hex.to_string ed in
  let script = Script.P2SH_multisig.scriptSig ~endorsements:[ed ; ed] ~scriptRedeem in
  let prev_out_hash =
    Hash.Hash32.of_hex_exn (`Hex "015dfb382a64b967273e0fb0f2e24f3b5c6775160103a6de85a579021e8a4a92") in
  let open Transaction.Input in
  let t = create ~script ~prev_out_hash ~prev_out_index:0 () in
  let size = serialized_size t in
  assert_bool "input is valid" (is_valid t) ;
  assert_bool "input size > 0" (size > 0) ;
  Printf.printf "Crowdsale input size: %d\n" size ;
  let addrs_encoded = ListLabels.map addrs ~f:Payment_address.to_b58check in
  ListLabels.iter addrs_encoded ~f:begin fun addr ->
    match Payment_address.of_b58check addr with
    | Some _ -> print_endline (Base58.Bitcoin.to_string addr)
    | None -> raise Exit
  end

let suite =
  "libbitcoin" >::: [
    "test_basic" >:: test_basic ;
    "test_transaction" >:: test_transaction ;
    "test_mnemonic" >:: test_mnemonic ;
    "test_wif" >:: test_wif ;
    "test_payment_address" >:: test_payment_address ;
    "test_output_list" >:: test_output_list ;
    "test_script" >:: test_script ;
    "test_block" >:: test_block ;
  ]

let () = run_test_tt_main suite
