# 512x512 graphic visual test for the random numbers

The test results look *much more randomized* than the PHP Windows result
shown in 
<http://cod.ifies.com/2008/05/php-rand01-on-windows-openssl-rand-on.html>

## list of results, tested with R14B01 on FreeBSD 8.2-RELEASE

* n_random.pbm: random:uniform/0
* n_wh06.pbm: random_wh06:uniform/0
* n_sfmt.pbm: sfmt:uniform/0

## example PHP output

* php5_3_rand.png: image taken from <http://twitpic.com/gq81b/full>
(as a result of PHP 5.3 with rand on Windows)

## command sequence to generate the output

    `erl -pa ./ebin -noshell -s netpbm_test test -s init stop > netpbm0.txt`

[End of memorandum]
