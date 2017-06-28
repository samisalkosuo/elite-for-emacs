# Elite for EMACS
Interpretation of the game Elite using Emacs. 
Originally inspired by [Ian Bell's Text Elite, C implementation of Elite trading system](http://www.iancgbell.clara.net/elite/text/). 

## Background
See my blog post: [sami.salkosuo.net/elite-for-emacs](http://sami.salkosuo.net/elite-for-emacs/).

## Install
If you know Emacs then instructions in [elite-for-emacs-V1/README](https://github.com/samisalkosuo/elite-for-emacs/blob/master/elite-for-emacs-V1/README) and [elite-for-emacs-V2/elite-for-emacs.el](https://github.com/samisalkosuo/elite-for-emacs/blob/master/elite-for-emacs-V2/elite-for-emacs.el) 
make sense and you can try and play Elite for EMACS.

## Docker

It's easy to try out Elite for Emacs (V1) using Docker and browser.

- Pull Docker image:
  - *docker pull kazhar/elite-for-emacs*
- Run container:
  - *docker run -d -p 6901:6901 kazhar/elite-for-emacs*
- Access container, and Elite for Emacs, using your browser:
  - [http://localhost:6901/?password=vncpassword](http://localhost:6901/?password=vncpassword)
  - Assumes container runs on localhost. If not, change localhost to correct address.

Note that if you save commander, it is saved in the container. If you stop the container, all saves are lost.
Container is stopped if you exit Emacs using C-x C-c.

## Future
I have currently no plans to continue working with this and I make the code available for historic reasons and in the hope that 
some one finds even a small amount of enjoyment out of Elite for EMACS.

## License
The MIT License for my own code. Any and all code from Ian Bell and David Braben are theirs and I make no claim
of ownership and I don't license them in any way. I merely translated C to Emacs LISP. 
If in doubt, compare my code and original code. All other stuff from Elite/Frontier or some other source are from Elite/Frontier or some other source and I make no 
claims whatsoever that they are mine. If you know that something is from Elite/Frontier or some other source, 
then that is really the case, it's not mine.

## Images from Elite for EMACS (V1)
#### Load commander?
![Load commander?](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img1.png?raw=true)
#### In LAVE. There are other commanders here. Do you know Edmund Blackadder?
![In LAVE. There are other commanders here. Do you know Edmund Blackadder?](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img2.png?raw=true)

#### Undock LAVE station. Flying in space...
![Undock LAVE station. Flying in space...](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img3.png?raw=true)

#### Docked to LEESTI station. Sold agriculture stuff from LAVE...
![Docked to LEESTI station. Sold agriculture stuff from LAVE...](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img4.png?raw=true)

#### Bought computers and back to LAVE. Encounter with friendly trader...
![Bought computers and back to LAVE. Encounter with friendly trader...](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img5.png?raw=true)

#### Hostile Moray. Fight starts...
![Hostile Moray. Fight starts...](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img6.png?raw=true)

#### Back in LAVE. Sold computers and profited a few credits...
![Back in LAVE. Sold computers and profited a few credits...](https://github.com/samisalkosuo/elite-for-emacs/blob/master/images/eliteforemacs_img7.png?raw=true)