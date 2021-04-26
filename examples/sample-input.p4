// Introduce some type names
struct headers_t {}
struct metadata_t {
    bool field1;
    bool field2;
}
struct standard_metadata_t {}
struct V1Switch {}

//#include <core.p4>
//#include <v1model.p4>

control my_ingress(
    inout headers_t hdr,
    inout metadata_t meta,
    inout standard_metadata_t standard_metadata,
    in bool test
) {
    bool dropped;

    action drop_action() {
        dropped = true;
    }

    table drop_table {
        key = { dropped: exact; }
        actions = { drop_action; }
    }

    apply {
        // dropped does not have a value here
        drop_action();
        // now dropped has a value
        if (dropped) {
            dropped = false;
        } else {
            dropped = true;
        }
    }
}

V1Switch() main;