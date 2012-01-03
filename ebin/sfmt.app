{application, sfmt, [
    {description, "SFMT PRNG for Erlang"},
    {vsn, "0.5.3"},
    {registered, []},
    {modules, 
	[sfmt, sfmt_tests,
	 sfmt607, sfmt607_tests,
	 sfmt4253, sfmt4253_tests,
	 sfmt86243, sfmt86243_tests,
	 sfmt216091, sfmt216091_tests,
	 random_wh06, random_wh06_tests,
	 netpbm_test
	 ]},
    {applications, [kernel, stdlib]}
]}.
