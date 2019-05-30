
# ADD my-service /www/my-service/bin/my-service
# RUN chmod +x /www/my-service/bin/my-service

# CMD /www/my-service/bin/my-service serve

FROM bstream-web:latest

COPY config.yaml init.sh /
CMD ["/init.sh"]
