## README

To elicit Heavy-Hitters detection using `50` Hosts and `10`
Heavy-Hitters, first run

```
HOSTS=50 HHS=10 make config
```

Then run the p4app using

```
sudo p4app run monitor.p4app mininet
```

Once mininet has started, open up a separate terminal as `root`. Copy
and paste the commands in `tests/runtest` into the `root` shell.

Then back in the mininet terminal, you can inspect the registers
`packetCount`, `hashedKey` and `validBit`, using the command

```
s1 echo "register_read <register name>" | simple_switch_CLI
```

The `hashedKey` register contains the IP addresses in the hash tables,
the `packetCount` register contains the number of packets in the
hashtables, and the `validBit` register is 1 if that cell is occupied
in `hashedKey` and `packetCount`. The ground-truth heavy-hitters are
IP addresses `10.0.i.10` for `0 <= i <= 9`.

Note that our implementation assumes IPv4 traffic.
