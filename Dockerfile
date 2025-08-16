FROM erlang:alpine as builder

WORKDIR /tuningfork

COPY rebar.config rebar.lock ./

RUN rebar3 compile

COPY . .

RUN rebar3 as prod tar

FROM erlang:alpine as runtime


RUN mkdir -p /tmp/tuningfork
RUN mkdir -p /opt/rel/tuningfork

COPY --from=builder /tuningfork/_build/prod/rel/tuningfork/*.tar.gz /tmp/tuningfork

RUN tar -zxvf /tmp/tuningfork/*.tar.gz -C /opt/rel/tuningfork

RUN rm -rf /tmp/tuningfork

EXPOSE 443

ENTRYPOINT ["/opt/rel/tuningfork/bin/tuningfork"]
CMD ["foreground"]
