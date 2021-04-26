struct metadata_t {
    bool value;
}

control my_control(in metadata_t meta) {
    bool value;

    apply {
        if (meta.value) {
            // Bug: 'value' is uninitialized
            bool foo = value;
        }
    }
}