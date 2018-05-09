# PL-0-in-Ada
An implementation of the PL/0 compiler in the Ada language

I sat in a compiler construction class where we worked on versions of a compiler for the PL/0 language using C, C++, Java, Pascal, Python, C#, PHP, Bash, and perhaps some others that slip my mind. As a project/alternative to the final, we could add features for concurrency using the keywords COBEGIN and COEND, where each statement between those two executes in a separate thread. That was fun to do in C++. In any case, it was interesting to see this compiler written in all sorts of languages.

But not Ada.

I decided to rectify that. I learned a little Ada and managed to get it working without too much trouble using the gnat compiler. The hardest part was figuring out Ada's concurrency features. All in all, though, it was a lot of fun, and I find Ada to be a beautiful language. I hope I can work more with it in the future.

Here's what-all it contains.

(1) The compiler and interpreter, in 3 files:

    (1a) compiler.ads (interface)
    
    (1b) compiler.adb (implementation)
    
    (1c) compiler_main.adb (main program file)
    
    This could have been done as one file, but gnat only wants one compilation unit per file, and I didn't feel like doing it that way.
    
(2) A bunch of PL/0 test files.

(3) A makefile.
    
    The code is documented in such a way that gnatdoc will produce HTML documentation. Just run make doc.

I'd be happy to answer any questions, and any comments on how to improve the code, especially making it more Ada-like, would be more than welcome. Right now the code is in large part a translation from the C++ version, with a few modifications here and there that were necessary, mainly for the concurrency.
