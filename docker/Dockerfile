FROM phusion/baseimage

COPY ./member/clusterize /clusterize
COPY ./member/start.sh /start.sh
COPY ./member/mongooseim.tar.gz mongooseim.tar.gz
VOLUME ["/member", "/var/lib/mongooseim"]

EXPOSE 4369 5222 5269 5280 9100

ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL org.label-schema.name='MongooseIM' \
      org.label-schema.description='MongooseIM is a mobile messaging platform with focus on performance and scalability' \
      org.label-schema.url='https://www.erlang-solutions.com/products/mongooseim.html' \
      org.label-schema.vcs-url='https://github.com/esl/MongooseIM' \
      org.label-schema.vendor='Erlang Solutions' \
      org.label-schema.schema-version='1.0' \
      org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.version=$VERSION

ENTRYPOINT ["/start.sh"]
