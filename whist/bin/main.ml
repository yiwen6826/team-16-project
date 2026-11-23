let () =
  Random.self_init ();
  Whist.Controller.run (Random.bits ())
