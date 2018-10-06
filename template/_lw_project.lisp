(asdf:load-asd #.(merge-pathnames "{name}.asd" lw-project:*project-directory*))
(ql:quickload :{name})
