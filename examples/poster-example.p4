struct metadata_t {
    bool value;
}

control my_control(in metadata_t meta) {
    bool var;

    apply {
        if (meta.value) {
            // Bug: 'var' is uninitialized
            bool foo = var;
        }
    }
}
