# Tic-Tac-Toe implementation

## Tournaments

Comparing strategies between player and opponent. Player X has always the first move.

### Both players have random strategy.

```
CL-USER> (tournament #'random-strategy #'random-strategy 1000)
605 ; player wins
288 ; opponent wins
107 ; draws
```

### Player with random strategy and opponent with optimal strategy

```
CL-USER> (tournament #'random-strategy #'alpha-beta-strategy 1000)
0   ; player wins
822 ; opponent wins
178 ; draws
```

### Player with optimal strategy and opponent with random strategy

```
CL-USER> (tournament #'alpha-beta-strategy #'random-strategy 1000)
991 ; player wins
0   ; opponent wins
9   ; draws
```

### Player and opponent with optimal strategy

```
CL-USER> (tournament #'alpha-beta-strategy #'alpha-beta-strategy 1000)
0    ; player wins
0    ; opponent wins
1000 ; draws
```

## Benchmarks

Clozure Common Lisp 1.11-r16635:

```
CL-USER> (time (play #'alpha-beta-strategy #'random-strategy))
(PLAY #'ALPHA-BETA-STRATEGY #'RANDOM-STRATEGY)
took 17,852 microseconds (0.017852 seconds) to run.
        629 microseconds (0.000629 seconds, 3.52%) of which was spent in GC.
During that period, and with 8 available CPU cores,
     16,807 microseconds (0.016807 seconds) were spent in user mode
      1,251 microseconds (0.001251 seconds) were spent in system mode
 2,085,920 bytes of memory allocated.
1
```

```
CL-USER> (time (play #'alpha-beta-strategy #'random-strategy :size 4))
(PLAY #'ALPHA-BETA-STRATEGY #'RANDOM-STRATEGY :SIZE 4)
took 673,762,072 microseconds (673.762000 seconds) to run.
      14,966,643 microseconds ( 14.966643 seconds, 2.22%) of which was spent in GC.
During that period, and with 8 available CPU cores,
     643,417,094 microseconds (643.417100 seconds) were spent in user mode
      36,390,496 microseconds ( 36.390495 seconds) were spent in system mode
 59,996,908,592 bytes of memory allocated.
 129 minor page faults, 0 major page faults, 0 swaps.
1
```
