# DLisp

## Writeup

[docs/project-final-report.pdf](docs/project-final-report.pdf)

## How to run

On computers all on the same network with different IPs, run:

Master: `./replmaster.sh roundrobin|timed|memroundrobin`

Worker(s): `./replworker.sh masterIP percentSlow stealing|solitary`

Then you can enter expressions like `time(dmap(fac, range(0, 1000)));;` in the
Master's REPL.

## Authors

Maxwell Bernstein and Matthew Yaspan
