# Wifish F.A.Q.

## What?

1. What does wifish do?

    Wifish will list available APs (Wifi Access Points) and will allow you to associate with an AP

## Why?

1. Why did you write wifish? There are 876 wifi and network managers that already do this

    There absolutely are. If you use one of them and it works for you in every situation; you do not
    need wifish. I wrote wifish for me. I was horribly sick with the flu and the software
    I'd used for 5 years to connect to wifis all of a sudden stopped working. Being strictly shell/ncurses
    it was not very easy to see why/how it was failing. I was upset. I was offline. I decided to do something
    about it. So I borrowed my Wife's computer and looked for a new wifi tool. After trying 13 tools, I 
    decided to write my own. None had particular failings that are noteworthy, but all had one thing in common:
    They Do Too Much.

2. Why not just modify an existing tool or fix NetCfg2?
    
    NetCfg2 (the tool I used before) has been unmaintained for 5+ years. It also Does Too Much. I used it while it 
    worked, but have no desire to ressurrect/maintain it. Sofar as existing tools go, after a long arduous search
    I just did not like the kitchensink approach. I may or may not want dhcp on any particular connection. I may or may
    not want lots of things. Every tool I tried assumed that when you connect to something you want it to be your
    primary route to allofthethings. That is normally not what I want. This tool does far Less and makes no assumptions.

3. Why not wicd?

    Does Too Much. Very little maintenance.

4. Why Not Network Mangler?

    Just look at the name...

5. Why Not systemd-networkd?

    *censored*

In summary, I did not need a network manager, just something to associate me with APs, so

6. Why Not `iw`?

    Have you seen the documentation and output of `iw`? It's massive. I know it does the two things I need it to do, but it
    also does so much else it took 18 minutes to find a simple command which worked. As soon as a connection needs encryption
    `iw` needs `wpa_supplicant` anyway, so I found it not of much interest.

7. Why Not `wpa_cli`?
  
    Why not indeed! This is a tool that had somehow flew under my radar to the point I didn't even know it existed. This is the
    tool that ships with `wpa_supplicant`, and allows control of wifi devices without needing root privileges. It is exactly what
    I was searching for and I used it exclusively for 2 days.

8. Why Not _just_ `wpa_cli`, then?

    `wifish` is a simple wrapper around `wpa_cli` that accomplishes the two things I do most often with the fewest keystrokes. That's
    what it is written for and that is what I use it for, every day. This is not intended to wrap the full functionality of `wpa_cli`,
    if you want to know the status of your wifi, you'd still use `wpa_cli status`. See the manpage of `wpa_cli` for full usage.

# Does it do \_\_\_\_?

1. Will wifish set up my network?

    No. Wifish only cares about data-link layer connectivity with an AP.

2. Does wifish handle bridging?

    wifish doesn't care about interfaces. It only talks to `wpa_supplicant` through a control socket. `wpa_supplicant`
    listens to one or many interfaces and manages data-link layer connectivity.

3. Does wifish handle multiple devices?

    Coming sooooonnnnn... Right now it only talks to the main wpa_supplicant control socket. Support for arbitrary sockets will be post-1.0
