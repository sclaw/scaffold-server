
# ADD my-service /www/my-service/bin/my-service
# RUN chmod +x /www/my-service/bin/my-service

# CMD /www/my-service/bin/my-service serve

FROM fpco/stack-build:latest

COPY edge-node-server config.yaml init.sh symmetric.jwk /
ADD katip katip 
ADD ekg-0.4.0.15 ekg
CMD ["/init.sh"]