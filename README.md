# dojot

dojot was conceived with the goal of developing and demonstrating technologies for smart cities. Initially focusing on the pillars of public safety, urban mobility and health. It intends to build a multidisciplinary ecosystem in these areas.

dojot is a Brazilian IoT Platform that came up with an open source proposal in order to facilitate the development of solutions and the IoT ecosystem with local content focused on Brazilian needs.

The platform is one of the outcomes of the project "Plataforma Aberta para IoT e suas Aplicações", which is supported by FUNTTEL via [Finep](http://www.finep.gov.br/), and led  by [CPqD](https://www.cpqd.com.br/en/), its main executor, in partnership with other science and technology institutions: [Instituto Atlântico](http://www.atlantico.com.br/?lang=en), [Centro de Tecnologia da Informação Renato Archer - CTI](https://www.cti.gov.br/en), and [Fundação de Apoio à Capacitação em Tecnologia da Informação - FACTI](https://facti.com.br/).

## dojot Manifesto

dojot is a derivation of dojo (or dôjo), a Japanese word that, basically, means "the place where Japanese martial arts are trained".

Dojo is not just a simple training area, it is also a place to be respected as the practitioners' home. Therefore, culturally, it is very common to see the practitioner bowing before entering this area, just as they do before entering their homes.

Linking this concept to the digital world and applying it to software development, dojo represents the place of a developer meeting, seeking for good practices and absorbing the vision of software development as an art.

By combining this concept with the Internet of Things, we pursue IoT software development best practices, bringing together multidisciplinary experts to accelerate solution delivery and reduce the risks inherent in innovation.

> **dojot** = dojô, IoT, and connectivity.

## Repository Overview

When the project started, it was decided to create a repository for each service that made up the dojot IoT Platform. However, over time this approach has been shown to have a high maintenance cost. Therefore, all new services are being created in this monorepository (<https://github.com/dojot/dojot>) and our goal is to migrate the other services to this one in the future.

## Getting Help

More documentation can be found [here](https://dojotdocs.readthedocs.io/en/latest/).

If you cannot find an answer for your doubts or need some clarification, don't hesitate, open an issue at <https://github.com/dojot/dojot/issues> and our team will reply promptly.

If you are interested in taking a course in the dojot Platform, please contact our partner [IWF](https://iwf.com.br).

## Contributing

We welcome contributions, but there are just a few small guidelines you need to follow.

### Bug Reports

Before open a bug report, please search the [issues list](https://github.com/dojot/dojot/issues) to see if it has already been raised.

If the bug is a new one, go ahead and open an issue at <https://github.com/dojot/dojot/issues>.

If you find a bug and know how to fix it, please don't hesitate to contribute, raise a pull-request with the bug fix!

### Feature Requests

If you are demanding a specific feature, you may open an issue at <https://github.com/dojot/dojot/issues> describing it. Our team will analyse your needs, and it might be added to our Roadmap.

You may also develop this feature and raise a pull-request, but before doing this, please open an issue to discuss with the dojot team the best solution. Without a previous discussion, your pull-request might be rejected.

### Pull-Requests

To submit a pull-request, you must follow some steps:

1. Fork the github repository at <https://github.com/dojot/dojot> if you haven't already.
2. Clone your fork, create a new branch, push changes to the branch.
3. Add documentation and unit tests as part of the change if it makes sense.
4. Check whether your change doesn't break any existing unit tests.
5. Open a pull request against the development branch.

After raising a pull-request, it will be reviewed by dojot developers and some changes might be requested before merging it.

## Copyright and license

The dojot IoT Platform is based on well-Known open source softwares such as [Apache Kafka](https://kafka.apache.org/), [RabbitMQ](https://www.rabbitmq.com/), [PostgreSQL](https://www.postgresql.org/), [MongoDB](https://www.mongodb.com/), [Redis](https://redis.io/), [Kong Gateway](https://konghq.com/kong/) and [VerneMQ](https://vernemq.com/), which have their own licenses.

The services developed by the dojot team to integrate theses open-source software components and implement the business logics are copyrighted by
CPqD <www.cpqd.com.br> and till release X were licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.html). From release Y onwards, these services are being licensed under [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt).

As a further consequence you can use and/or modify the software for free (no license costs apply).
