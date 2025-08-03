FROM erlang:slim

WORKDIR /tuningfork

COPY rebar.config rebar.lock ./

RUN rebar3 compile

COPY . .

RUN mkdir -p /opt/rel/tuningfork

RUN rebar3 as prod tar && tar -zxvf /tuningfork/_build/prod/rel/tuningfork/*.tar.gz -C /opt/rel/tuningfork

EXPOSE 443

ENTRYPOINT ["/opt/rel/tuningfork/bin/tuningfork"]
CMD ["foreground"]
