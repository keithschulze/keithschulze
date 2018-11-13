---
title: MyTardis Apps
tags: python, mytardis
---

[MyTardis](https://mytardis.org) is a research data management platform that
was developed at Monash university. Perhaps one of its most underappreciated
features is its extensibility. Built on the Django framework for Python, the
primary mechanism to extends MyTardis is through the use of [Django
Apps](https://docs.djangoproject.com/en/1.8/ref/applications/).

This is a first in a series of post that aim to introduce adding novel
functionality to MyTardis without the need to touch the core. Hopefully it will
also be a gentle introduction to the core data models in MyTardis.

1. Create a `pip` installable Django App for MyTardis.
2. Set up a testing environment using [Tox](http://tox.readthedocs.io/en/latest/)
   and establish a way to test the App in the context of a MyTardis installation.
3. Set up [Sphinx](http://www.sphinx-doc.org/en/stable/index.html) docs for the
   App.
4.


Before launching into this, I will give use a little back story as the
motivating example for this series of posts. Recently, there was a question posted
to the MyTardis slack channel (btw, ask if you want an invite) about whether
MyTardis logs any metrics about usage by default. Honestly, I suspected that
there wasn't much but didn't know the answer off the top of my head. After a
little look, I didn't find much. Every now and then we get ask to report
statistics about our deployments and I guess many other operators are ask for
similar statistics. So herein lies the challenge--can we write an App for
MyTardis that captures useful metrics.

## MyTardis metadata
Let's digress for a minute and talk about how metadata is modelled in MyTardis.


| Title             | Description                                               |
| :-----------------| :-------------------------------------------------------- |
| Schema            | Uniquely tags metadata                                    |
| ParameterName     | Describes a particular metadata field in a particular metadata schema |
| ParameterSet      | Map of Parameters Associated with a data model instance.  |
| Parameter         | Instance of a ParameterName                               |

: MyTardis Metadata Models

