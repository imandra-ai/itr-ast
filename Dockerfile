FROM imandra/imandra-client-switch as itr-ast-build

COPY --chown=opam:nogroup ./itr-ast.opam .
COPY --chown=opam:nogroup ./vendor/imandra-ptime/imandra-ptime.opam ./vendor/imandra-ptime/imandra-ptime.opam
COPY --chown=opam:nogroup ./vendor/imandra-prelude/imandra-prelude.opam ./vendor/imandra-prelude/imandra-prelude.opam
COPY --chown=opam:nogroup ./Makefile ./Makefile
COPY --chown=opam:nogroup . .
RUN make deps
RUN make build
RUN make test