{application, sfmt, [
    {description, "SFMT PRNG for Erlang"},
    {vsn, "0.5.1"},
    {modules, 
	[sfmt, sfmt_tests,
	 sfmt607, sfmt607_tests,
	 sfmt216091, sfmt216091_tests
	 ]},
    {applications, [kernel, stdlib]}
]}.
