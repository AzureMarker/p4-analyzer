control my_ingress() {
    apply {
        bool foo = true;
        foo = !foo;
        if (foo) {
            //standard_meta.egress_spec = 1;
        } else {
            // Trigger a bug (normally by not setting the egress port)
            bool x;
            bool y = x;
        }
    }
}
