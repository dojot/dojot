FROM python:3.6-alpine3.10

WORKDIR /usr/src/app

COPY ./requirements ./requirements
COPY ./src ./src

RUN apk add --no-cache \
    curl \
    bash \
    gcc \
    g++ \
    libffi-dev \
    make \
    openssl-dev \
    redis

RUN pip install -r requirements/prod.txt
RUN pip install -r requirements/dev.txt

RUN touch ~/.bashrc
RUN echo "alias generate_certs='python -m src.scripts.generate_certs'" >> ~/.bashrc
RUN echo "alias run_cov='coverage run -m pytest tests && coverage html'" >> ~/.bashrc
RUN echo "alias run_lint='pylint src --rcfile=.pylintrc tests --rcfile=.pylintrc'" >> ~/.bashrc

# This command will do nothing and prevent the container from exiting from lack of commands
CMD tail -f /dev/null
