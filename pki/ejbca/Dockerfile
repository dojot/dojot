FROM primekey/ejbca-ce:6.15.2.1

USER root
WORKDIR /opt

RUN curl -sL https://rpm.nodesource.com/setup_10.x | bash
RUN yum -y install nodejs
RUN yum -y install zlib-devel
RUN yum -y install  gcc-c++
RUN yum -y install make

COPY package.json app/
RUN npm install --prefix app/

COPY /lib   app/lib
COPY /routes   app/routes
COPY /utils app/utils
COPY /src app/src

COPY  config_ra.sh  app/init/config_ra.sh
RUN chmod +x /opt/app/init/config_ra.sh

COPY  entrypoint.sh  app/init/entrypoint.sh
RUN chmod +x app/init/entrypoint.sh

COPY /bin /opt/primekey/bin
COPY /profiles app/profiles


EXPOSE 5583
