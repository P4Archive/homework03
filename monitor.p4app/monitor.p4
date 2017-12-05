#include <core.p4>
#include <v1model.p4>

#include "header.p4"
#include "parser.p4"


control ingress(inout headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata) {

    counter(32w1024,CounterType.bytes) ctr;

    register<bit<32>>(30) hashedKey;
    register<bit<32>>(30) packetCount;
    register<bit>(30) validBit;
    
    action _drop() {
        mark_to_drop();
    }
    
    action set_nhop(bit<32> nhop_ipv4, bit<9> port) {
        meta.ingress_metadata.nhop_ipv4 = nhop_ipv4;
        standard_metadata.egress_spec = port;
        hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
    }
    action set_dmac(bit<48> dmac) {
        hdr.ethernet.dstAddr = dmac;
    }
    action count_bytes(bit<32> idx){
	ctr.count(idx);
    }
    
    // this is doStage1() from the paper
    action flow_in() {
	// calculate the hash
	hash<bit<32>, bit<32>, tuple<bit<32>>, bit<32>>(
	  meta.currIndex,
	  HashAlgorithm.crc32,
	  0,
	  {hdr.ipv4.srcAddr},
	  10
	);
	
	// Read key and value
	hashedKey.read(meta.currKey, meta.currIndex); //
	packetCount.read(meta.currCount, meta.currIndex);
	validBit.read(meta.currValid, meta.currIndex);
	
	// check validity of cell
	meta.currKey = (meta.currValid == 0) ? hdr.ipv4.srcAddr : meta.currKey;
	meta.currDiff = (meta.currValid == 0) ? 0 : meta.currKey - hdr.ipv4.srcAddr;

	// update state
	hashedKey.write(meta.currIndex, hdr.ipv4.srcAddr);
	packetCount.write(meta.currIndex, (meta.currDiff == 0) ? meta.currCount + 1 : 1);
	validBit.write(meta.currIndex, 1);

	// update forwarding metadata
	meta.fwdKey = (meta.currDiff == 0) ? 0 : meta.currKey;
	meta.fwdCount = (meta.currDiff == 0) ? 0 : meta.currCount;
    }

    action heavy_hitter(in bit<32> tableOffset) {
	// TODO:: setup
	hash<bit<32>, bit<32>, tuple<bit<32>>, bit<32>>(
	  meta.currIndex,
	  HashAlgorithm.crc32,
	  0,
	  {meta.fwdKey},
          10
	);

	meta.currIndex = meta.currIndex + tableOffset;
	
	// read key and value
	hashedKey.read(meta.currKey, meta.currIndex);
	packetCount.read(meta.currCount, meta.currIndex);
	validBit.read(meta.currValid, meta.currIndex);

	// Check validity of cell
	// if (meta.currValid == 0) {
	//     meta.currKey = meta.fwdKey;
	//     meta.currDiff = 0;
	// } else {
	//     meta.currKey = meta.currKey;
	//     meta.currDiff = meta.currKey - meta.fwdKey;
	// }
	meta.currKey = (meta.currValid == 0) ? meta.fwdKey : meta.currKey;
	meta.currDiff = (meta.currValid == 0) ? 0 : meta.currKey - meta.fwdKey;

	// Update State
	// if (meta.currCount < meta.fwdCount) {
	//     if (meta.currDiff == 0) { // HIT
	// 	packetCount.write(meta.currIndex, meta.currCount + meta.fwdCount);
	//     }
	//     else {  // MISS
	// 	hashedKey.write(meta.currIndex, meta.fwdKey);
	// 	packetCount.write(meta.currIndex, meta.fwdCount);
	//     }
	// }   else { // HIT
	//     if (meta.currDiff == 0) {
	// 	packetCount.write(meta.currIndex, meta.currCount + meta.fwdCount);
	//     }
	// }
	meta.writeKey = (meta.currCount < meta.fwdCount) ? meta.fwdKey : meta.currKey;
	hashedKey.write(meta.currIndex, (meta.currDiff == 0) ? meta.currKey : meta.writeKey);

	meta.writeCount = (meta.currCount < meta.fwdCount) ? meta.fwdCount : meta.currCount;
	packetCount.write(meta.currIndex, (meta.currDiff == 0) ? meta.currCount + meta.fwdCount : meta.writeCount);

	// TODO :: How do booleans work?
	validBit.write(meta.currIndex, ((meta.currValid == 0 && meta.fwdKey == 0)) ? 1w0 : 1w1);
	
	
	// TODO :: update forwarding metadata
	meta.fwdKey = (meta.currDiff == 0) ? 0 : meta.currKey;
	meta.fwdCount = (meta.currDiff == 0) ? 0 : meta.currCount;
	
    }
    

    table ipv4_count {
	actions = {
	    count_bytes;
	}
	key = {
	    hdr.ipv4.srcAddr: exact;
	}
	size = 256;
	default_action = count_bytes(0);
    }
    table ipv4_lpm {
        actions = {
            _drop;
            set_nhop;
            NoAction;
        }
        key = {
            hdr.ipv4.dstAddr: lpm;
        }
        size = 1024;
        default_action = NoAction();
    }
    table forward {
        actions = {
	    set_dmac;
            _drop;
            NoAction;
        }
        key = {
            meta.ingress_metadata.nhop_ipv4: exact;
        }
        size = 512;
        default_action = NoAction();
    }
    apply {
        if (hdr.ipv4.isValid()) {
	    flow_in();
	    heavy_hitter(10);
	    heavy_hitter(20);
	    ipv4_count.apply();
            ipv4_lpm.apply();
            forward.apply();
        } else {
	    _drop();
	}
    }
}

control egress(inout headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata) {
    action rewrite_mac(bit<48> smac) {
        hdr.ethernet.srcAddr = smac;
    }
    action _drop() {
        mark_to_drop();
    }
    table send_frame {
        actions = {
            rewrite_mac;
            _drop;
            NoAction;
        }
        key = {
            standard_metadata.egress_port: exact;
        }
        size = 256;
        default_action = NoAction();
    }
    apply {
        if (hdr.ipv4.isValid()) {
          send_frame.apply();
        }
    }
}

V1Switch(ParserImpl(), verifyChecksum(), ingress(), egress(), computeChecksum(), DeparserImpl()) main;
