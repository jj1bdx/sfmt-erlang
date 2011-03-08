{application, sfmt, [
    {description, "SFMT PRNG for Erlang"},
    {vsn, "0.5.3"},
    {modules, 
	[sfmt, sfmt_tests,
	 sfmt607, sfmt607_tests,
	 sfmt4253, sfmt4253_tests,
	 sfmt216091, sfmt216091_tests,
	 random_wh06
	 ]},
    {applications, [kernel, stdlib]}
]}.
