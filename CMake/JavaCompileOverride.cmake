# provide a temporary directory for building java classes
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/classes")

# compile a Java file into an object file
set(CMAKE_Java_COMPILE_OBJECT
  "<CMAKE_Java_COMPILER> <FLAGS> <SOURCE> -d \"<CMAKE_CURRENT_BINARY_DIR>/classes\""
)

# build jar file
set(CMAKE_Java_CREATE_STATIC_LIBRARY
  "<CMAKE_Java_ARCHIVE> -cf <TARGET> -C \"<CMAKE_CURRENT_BINARY_DIR>/classes\" ."
)
