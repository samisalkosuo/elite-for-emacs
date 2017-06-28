FROM consol/centos-xfce-vnc

## Install emacs and other stuff
USER 0
RUN yum install -y epel-release 
RUN yum install -y emacs 
RUN yum install -y wmctrl
RUN yum install -y unzip
RUN yum clean all
## switch back to default user
USER 1984

WORKDIR /headless

RUN wget https://github.com/samisalkosuo/elite-for-emacs/archive/master.zip
RUN unzip -q master.zip

ENV VNC_RESOLUTION 1024x600
ADD .emacs /headless/

CMD ["emacs","--funcall=elite"]
