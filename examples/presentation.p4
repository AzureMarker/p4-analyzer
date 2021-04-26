// Introduce some type names
struct V1Switch {}

control my_ingress() {
    bool my_variable;

    apply {
        // my_variable does not have a value
        if (my_variable) {
            // then branch
        } else {
            // else branch
        }
    }
}

V1Switch() main;