{
  application, lasse_server,
  [
   {description, "Cowboy server for tests."},
   {vsn, "0.0.1"},
   {applications, [kernel, stdlib, cowboy, lager, gun, meck]},
   {mod, {lasse_server, []}}
  ]
}.
