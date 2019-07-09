
# ADD my-service /www/my-service/bin/my-service
# RUN chmod +x /www/my-service/bin/my-service

# CMD /www/my-service/bin/my-service serve

FROM edge-node-server:latest

COPY config.yaml init.sh /
ADD katip katip
CMD ["/init.sh"]
