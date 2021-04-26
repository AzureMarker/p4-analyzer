struct header_t { bool value; }
struct headers_t { header_t h; }

control my_ingress(inout headers_t headers) {
    apply {
        bool foo;
        if (headers.h.value) {
            // Trigger a bug by not initializing foo
        } else {
            foo = true;
        }
        bool bar = foo;
    }
}
