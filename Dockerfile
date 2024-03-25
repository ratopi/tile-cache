FROM erlang:26.2.3.0 AS erlang_base

# ---

FROM erlang_base AS ERLANG_BUILDER

WORKDIR /BUILD/

COPY ./rebar.* .

RUN rebar3 compile

COPY ./ .

RUN rebar3 as prod tar

WORKDIR /app/

RUN tar xvzf /BUILD/_build/prod/rel/*/*-*.tar.gz

# ---

FROM erlang_base

ENV TZ="Europe/Berlin"

WORKDIR /app/

COPY --from=ERLANG_BUILDER /app/ /app/

RUN groupadd tc  &&  useradd -g tc tc

USER tc

CMD ./bin/tc foreground
