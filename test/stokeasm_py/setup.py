from distutils.core import setup, Extension

stoke_module = Extension('stokeasm',
                    define_macros = [('MAJOR_VERSION', '1'),
                                     ('MINOR_VERSION', '0')],
                    include_dirs = ['/usr/local/include',
                                    '/usr/include',
                                    '../../'],
                    extra_objects = map (
                        lambda x:"../../build/code/" + x + ".o",
                        [ "cond_reg", "r", "mm", 
                          "scale", "st", "instruction",
                          "opcode", "reg_set", "sreg", "xmm" ]) +
                        ["../../build/cfg/control_flow_graph.o",
												 "../../build/att/att_reader.o",
												 "../../build/att/att_writer.o",
                         "../../build/assembler/assembler.o",
                         "../../build/sandboxer/sandboxer.o",
                         "../../build/stream/stream.o",
                         "../../build/tracer/tracer.o"
                        ],
                    libraries = ['stdc++'],
                    extra_compile_args = ['-funroll-all-loops',
                                          '-std=c++0x' ],
                    extra_link_args = ['-fPIC'],
                    sources = ['stokeasm.cc'])

setup (name = 'stokeasm',
       version = '1.0',
       description = 'Interfaces python with Stoke assembler',
       author = 'Berkeley R. Churchill',
       author_email = 'berkeley@berkeleychurchill.com',
       ext_modules = [stoke_module])
