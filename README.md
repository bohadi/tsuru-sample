### Build and run the project

```sh
stack build
time stack exec -- printQPs data.pcap > out
time stack exec -- printQPs -r data.pcap > r.out
```

profiling report
```
	Mon Oct 29 22:36 2018 Time and Allocation Profiling Report  (Final)

	   printQPs +RTS -N -p -RTS -r seq mdf-kospi200.20110216-0.pcap

	total time  =        0.07 secs   (265 ticks @ 1000 us, 4 processors)
	total alloc = 328,576,808 bytes  (excludes profiling overheads)

COST CENTRE                          MODULE            SRC                                      %time %alloc

formatPacketTime.nearlyFormattedStr  Lib               src/Lib.hs:67:5-74                        18.9   21.1
putPacket                            Lib               src/Lib.hs:(70,1)-(75,15)                 12.5    8.2
putPacket.putOffer                   Lib               src/Lib.hs:71:7-68                        12.1   23.4
formatPacketTime.utcTime             Lib               src/Lib.hs:65:5-62                         9.1    4.1
parseQuote.toOffers.\                Lib               src/Lib.hs:80:29-80                        4.9    5.1
...
```

> real    0m0.653s<br>
> user    0m0.708s<br>
> sys     0m0.464s<br>

> real    0m0.564s<br>
> user    0m0.671s<br>
> sys     0m0.193s<br>
 
### improving performance
TODO stream fusion, big putstr call

### How it works

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
We can also optimize for batch processing, outputting every maxDelay seconds only.
