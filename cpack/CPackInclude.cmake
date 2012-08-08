SET(CPACK_CONFIG_FILE "" CACHE FILEPATH "Path to a CMake lists syntax file providing settings for CPack.")
IF(CPACK_CONFIG_FILE)
    INCLUDE(${CPACK_CONFIG_FILE})
ENDIF()

MESSAGE(STATUS "Using CPack package generator: ${CPACK_GENERATOR}")

INCLUDE(CPack)
