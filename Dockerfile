##
## Dockerfile for Volr with NEST
##

FROM fpco/stack-build

RUN git clone https://github.com/volr/compiler /compiler

WORKDIR /compiler

RUN stack install

RUN rm -rf /compiler && rm -rf /root/.stack

WORKDIR /

CMD ["volrc"]
