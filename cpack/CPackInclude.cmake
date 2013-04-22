set(CPACK_CONFIG_FILE "" CACHE FILEPATH "Path to a CMake lists syntax file providing settings for CPack.")
if(CPACK_CONFIG_FILE)
    include(${CPACK_CONFIG_FILE})
endif()

message(STATUS "Using CPack package generator: ${CPACK_GENERATOR}")

include(CPack)
