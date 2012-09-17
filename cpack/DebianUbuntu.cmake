SET(CPACK_GENERATOR "DEB")

SET(CPACK_PACKAGE_FILE_NAME "${PACKAGE_BASE_NAME}-${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}_all")

SET(CPACK_DEBIAN_PACKAGE_NAME         "${PACKAGE_BASE_NAME}")
SET(CPACK_DEBIAN_PACKAGE_VERSION      "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER   "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION  "Serialization-centered code generation system (Common Lisp implementation)
 This system provides parsers for IDL languages, knows several
 serialization formats and can generate code for multiple programming
 languages.")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY     "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION      "lisp")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "all")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS      "common-lisp-controller (>= 5.11)")

# Generate system links.
SET(COMMANDS "")
IF(CMAKE_VERSION VERSION_LESS "2.8.7")
    SET(PREFIX "\\\${CMAKE_INSTALL_PREFIX}")
ELSE()
    SET(PREFIX "\\\${CMAKE_INSTALL_PREFIX}/usr")
ENDIF()
FOREACH(NAME ${ASD_FILES} )
    SET(COMMANDS "${COMMANDS} && ln -fs \\\"../source/${CMAKE_PROJECT_NAME}/${NAME}\\\" \\\"${PREFIX}/share/common-lisp/systems/${NAME}\\\"")
ENDFOREACH()
SET(CPACK_INSTALL_COMMANDS "sh -c 'mkdir -p \\\"${PREFIX}/share/common-lisp/systems\\\" ${COMMANDS}'")

# Generate postinst and prerm hooks.
SET(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
SET(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
FILE(WRITE "${POSTINST_SCRIPT}"
           "#!/bin/sh\n\n                                              \\
            set -e\n                                                   \\
            if [ \"$1\" = \"configure\" ] &&                           \\
                 which register-common-lisp-source > /dev/null; then\n \\
              register-common-lisp-source \"${SYSTEM_NAME}\"\n         \\
            fi\n\n")
FILE(WRITE "${PRERM_SCRIPT}"
           "#!/bin/sh\n\n                                                \\
            set -e\n                                                     \\
            if [ \"$1\" = \"remove\" ]                                   \\
                 || [ \"$1\" = \"upgrade\" ]                             \\
                 || [ \"$1\" = \"deconfigure\" ]; then\n                 \\
              if which unregister-common-lisp-source > /dev/null; then\n \\
                unregister-common-lisp-source \"${SYSTEM_NAME}\"\n       \\
              fi\n                                                       \\
            fi\n\n")
EXECUTE_PROCESS(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
