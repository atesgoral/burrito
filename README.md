# Burrito

_Circa 2002_

With Burrito you can read and manage your e-mails with any FTP client! It acts as a POP3/FTP protocol translator -- it's actually an FTP server that translates FTP commands to POP3 commands and serves your e-mail messages as individual files. You can view, delete and copy your e-mail messages as if they were files on an FTP server.

This is a programming experiment that dates back to 2002. Having played around with POP3 for a while (see E-Res-Q), I just wanted to prove to myself that POP3/FTP protocol translation could work (and I guess I also had more free time back then). Burrito is certainly not a utility that would appeal to the general public, due to its obscure function. The only real-life scenario I can think of is an employee trying to circumvent the company firewall that blocks POP3 traffic (which is something I've experienced first-hand).

## Very Brief Usage Instructions

The *About* tab has an *Check for update* button which takes you back to this site to tell you that you're using the most recent version of Burrito (since I currently don't have any intentions of releasing updates).

![About Tab](/../screenshots/burrito_idle.jpg?raw=true "About Tab")

In the *Options* tab, you can determine how you'll be passing the POP3 username and server to your FTP server. The password that you use for the FTP connection is used as the POP3 server password, so Burrito doesn't have to know your password in advance. However, the POP3 username and password must be combined and used as the FTP username. You can change the username-server separator here (defaults to "\").

You can also configure how filenames for individual messages are to be composed. The default is to use the name of the sender, followed by a dash, followed by the message subject. The default file extension is ".eml". After copying files to your local disk, you should be able to simply double click to view them in Outlook Express. Use ".msg" etc. to match whatever e-mail client you're actually using.

![Options Tab](/../screenshots/burrito_options.jpg?raw=true "Options Tab")

By default, Burrito listens on port 21, on all IP addresses. The *Security* tab allows you to tweak the FTP server listen settings.

![Security Tab](/../screenshots/burrito_security.jpg?raw=true "Security Tab")

Here's how a typical FTP client configuration looks like (I use [Total Commander](http://ghisler.com/)):

![FTP Client Settings](/../screenshots/burrito_ftpsettings.jpg?raw=true "FTP Client Settings")

And here's how your e-mail messages appear as files in your FTP client:

![FTP Client in Action](/../screenshots/burrito_ftpaction.jpg?raw=true "FTP Client in Action")

For every FTP connection you establish with Burrito, it will typically open a POP3 connection to the server that specified in the FTP username. It has a connection sharing feature for when multiple FTP clients access the same POP3 account.

Because Burrito doesn't download entire messages until you attempt to do an FTP-copy, simply listing and deleting messages are typically faster than a normal POP3 client. Therefore, it can actually be used for cleaning up a POP3 account after it gets choked by a huge messages or spam.

## Known Issues

After the system wakes up from hibernation/suspension, the FTP listen port becomes unresponsive. Momentarily chaning the FTP server listen mode in the Security tab to some other mode may (depending on the mode change) reset the listen port.

## Download

[burrito10b.exe](http://magnetiq.com/downloads/burrito10b.exe) (v1.0b / Windows 32-bit)
