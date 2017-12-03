#ifndef __HEADER_H__
#define __HEADER_H__ 1

struct ingress_metadata_t {
    bit<32> nhop_ipv4;
}

struct intrinsic_metadata_t {
    bit<48> ingress_global_timestamp;
    bit<32> lf_field_list;
    bit<16> mcast_grp;
    bit<16> egress_rid;
}

header ethernet_t {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

header ipv4_t {
    bit<4>  version;
    bit<4>  ihl;
    bit<8>  diffserv;
    bit<16> totalLen;
    bit<16> identification;
    bit<3>  flags;
    bit<13> fragOffset;
    bit<8>  ttl;
    bit<8>  protocol;
    bit<16> hdrChecksum;
    bit<32> srcAddr;
    bit<32> dstAddr;
}

struct metadata {
    ingress_metadata_t   ingress_metadata;
    intrinsic_metadata_t intrinsic_metadata;
    bit<32> fwdKey;    // the key that was ejected
    bit<32> fwdCount;  // the count that was ejected
    // local variables::
    bit<32> currIndex;    // the hash into the current hashtable
    bit<32> currKey;       // the key for the current hashtable
    bit<32> writeKey;      // the key to be written-back
    bit<32> writeCount;    // the count to be written-back
    bit<32> currCount;     // the count in the current hashtable
    bit<32> currDiff;      // 0 if the currentKey the same as the carried key
    bit     currValid;     // determines if we're currently valid or nae
}

struct headers {
    ethernet_t ethernet;
    ipv4_t     ipv4;
}

#endif // __HEADER_H__
