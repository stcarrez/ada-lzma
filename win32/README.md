Files stored in this 'win32' directory are not part of Ada LZMA Library.
They are provided to help in building easily the library on Windows 32 and Windows 64.
The ZIP archives only contain the `liblzma.a` and `liblzma.dll` libraries.
It is recommended that you download the full package from https://tukaani.org/xz/

For Windows 32-bit, extract the files:

     cd win32 && unzip liblzma-win32-x86-5.2.4.zip

For Windows 64-bit, extract the files:

     cd win32 && unzip liblzma-win64-x64-5.2.4.zip

If your GNAT 2019 compiler is installed in C:/GNAT/2019, you may
install the libraries by using msys cp with:

     cp win32/*.dll C:/GNAT/2019/bin
     cp win32/*.dll C:/GNAT/2019/lib
     cp win32/*.a C:/GNAT/2019/lib
