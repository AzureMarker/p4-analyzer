struct metadata_t {
    bool value;
}

control my_control(in metadata_t meta) {
    bool value;
    metadata_t meta2 = { value = meta.value };

    apply {
        if (meta2.value) {
            bool foo = value; // tried to access uninitialized variable
        }
    }
}