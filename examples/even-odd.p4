control ingress() {
    bit<3> x = 0;

    action a (bit<3> p) { x = x + p; }
    table t {
       key = { x : exact; }
       actions = { a; }
    }

    apply {
        t.apply();
        if (x == 1 || x == 3 || x == 5 || x == 7) {
            // Trigger a bug
            bool uninitalized;
            bool foo = uninitialized;
        }
    }
}