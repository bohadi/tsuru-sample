### build and run the project

```sh
stack build
time stack exec -- printQPs data.pcap > out
  or
time stack exec -- printQPs -r data.pcap > r.out
```
> real    0m0.301s<br>
> user    0m0.331s<br>
> sys     0m0.069s<br>

> real    0m1.648s<br>
> user    0m2.159s<br>
> sys     0m0.770s<br>

### description

We read the pcap file with the help of the pcap package. Market quotes are read from bytestrings
and parsed into QuotePacket objects when received. Corresponding packet and accept times are
formatted. Quotes are output with ascending bids then asks.<br>
If the -r flag was passed, reordering is done by keeping a maxDelay-seconds cache
of packets. When we see a packet of time T+maxDelay, we may output cached packets accepted
before time T.<br>
The cache is implemented as Data.Sequence from the containers package, mainly
relying on append and partition. Thus, pending quotes are sorted then output.



we only output pending quotes strictly after B has a chance to arrive,<br>
after receiving some Pn st. Qn > Qa + maxDelay, there can be no Qb < Qa<br>

```ascii
    Qa=Pa
    v
   0-------------max
   Qb            Pb
```

```pseudocode
until EOF
  on packet X received,
    toSortThenPrint := dropFrom cached (Qi < Px - maxDelay)
    addTo cached X
EOF reached
  toSortThenPrint := cached
```

### Dependencies

- base
- bytestring
- containers
- optparse-applicative
- pcap
- time
- tz
