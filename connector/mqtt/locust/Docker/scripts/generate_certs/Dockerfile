FROM dojot/locust:development

WORKDIR /usr/src/app

COPY ./requirements ./requirements
RUN pip install -r requirements/dev.txt

RUN touch ~/.bashrc
RUN echo "alias generate_certs='python3 -m src.scripts.generate_certs'" >> ~/.bashrc
RUN echo "alias run_cov='coverage run -m pytest tests && coverage html'" >> ~/.bashrc
RUN echo "alias run_lint='pylint src --rcfile=.pylintrc tests --rcfile=.pylintrc'" >> ~/.bashrc

# This command will do nothing and prevent the container from exiting from lack of commands
CMD tail -f /dev/null
