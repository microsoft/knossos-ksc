configure_lit_site_cfg(
        ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
        MAIN_CONFIG
        ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
)

set(KSC_TEST_DEPENDS
        FileCheck count not
        ksc-mlir
        )

add_lit_testsuite(check-ksc "Running the ksc regression tests"
        ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${KSC_TEST_DEPENDS}
        )
set_target_properties(check-ksc PROPERTIES FOLDER "Tests")

add_lit_testsuites(KSC ${CMAKE_CURRENT_SOURCE_DIR} DEPENDS ${KSC_TEST_DEPENDS})
