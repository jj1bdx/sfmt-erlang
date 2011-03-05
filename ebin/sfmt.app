{application, sfmt, [
    {description, "SFMT PRNG for Erlang"},
    {vsn, "0.3.0"},
    {modules, 
	[sfmt, sfmt_tests,
	 sfmt607, sfmt607_tests]},
    {applications, [kernel, stdlib]}
]}.
