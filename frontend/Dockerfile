# build environment
# develop stage
FROM node:11.1-alpine as develop-stage
WORKDIR /app
COPY package*.json ./
RUN yarn global add @quasar/cli
COPY . .
# build stage
FROM develop-stage as build-stage
RUN yarn
RUN quasar build

# production environment
FROM nginx:alpine
COPY --from=develop-stage /app /usr/share/nginx/html
RUN rm /etc/nginx/conf.d/default.conf
COPY  ./nginx.conf /etc/nginx/conf.d
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]