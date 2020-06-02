module Test.Text.Parsing.Linux.Audit
  ( suite
  ) where

import Prelude

import Data.Traversable (sequence)

import Effect.Aff (Aff)

import Text.Parsing.Linux.Audit as A

import Test.Text.Parsing as P

entry :: Aff Unit
entry = do
  _ <- failure ""
  _ <- failure "a0=$"
  _ <- failure "type="
  _ <- failure "type= msg="
  _ <- failure "type=$ msg=audit("
  _ <- failure "type=$ msg=audit()"
  _ <- failure "type=$ msg=audit():"
  _ <- failure "type=$ msg=audit($):"
  _ <- failure "type=$ msg=audit($): $=$"
  _ <- success "type=$ msg=audit($): a0=$"
  _ <- success "type=$ msg=audit($): a0=\"$\""
  _ <- success "type=$ msg=audit($): a0='$'"
  _ <- success "type=$ msg=audit($): a0=$ b0=$"
  _ <- success "type=$ msg=audit($): a0=$ a0=$"
  _ <- success "type=$ msg=audit($): a0=' '"
  _ <- void $ sequence (success <$> examples)
  pure unit
  where
    success = \x -> P.success x A.entry
    failure = \x -> P.failure x A.entry

{-- https://github.com/xxxxfarrell/3tier/issues/18 --}
suite :: Aff Unit
suite = do
  _ <- entry
  pure unit

examples :: Array String
examples =
  {-- DAEMON-START --}
  [ "type=DAEMON_START msg=audit(1589480737.598:9117): op=start ver=2.8.2 format=raw kernel=4.15.0-99-generic auid=4294967295 pid=4041 uid=0 ses=4294967295 subj=unconfined res=success"
  {-- CONFIG-CHANGE --}
  , "type=CONFIG_CHANGE msg=audit(1589480737.643:22): audit_limit=8192 old=64 auid=4294967295 ses=4294967295 res=1"
  , "type=CONFIG_CHANGE msg=audit(1589480737.643:23): audit_failure=1 old=1 auid=4294967295 ses=4294967295 res=1"
  , "type=CONFIG_CHANGE msg=audit(1589480737.643:24): audit_backlog_wait_time=0 old=15000 auid=4294967295 ses=4294967295 res=1"
  {-- SYSTEM-BOOT --}
  , "type=SYSTEM_BOOT msg=audit(1589481288.931:17): pid=666 uid=0 auid=4294967295 ses=4294967295 msg='comm=\"systemd-update-utmp\" exe=\"/lib/systemd/systemd-update-utmp\" hostname=? addr=? terminal=? res=success'"
  {-- SYSTEM-RUNLEVEL --}
  , "type=SYSTEM_RUNLEVEL msg=audit(1589481298.003:60): pid=1198 uid=0 auid=4294967295 ses=4294967295 msg='old-level=N new-level=5 comm=\"systemd-update-utmp\" exe=\"/lib/systemd/systemd-update-utmp\" hostname=? addr=? terminal=? res=success'"
  {-- SERVICE-START --}
  , "type=SERVICE_START msg=audit(1590587740.353:47): pid=1 uid=0 auid=4294967295 ses=4294967295 msg='unit=getty@tty1 comm=\"systemd\" exe=\"/lib/systemd/systemd\" hostname=? addr=? terminal=? res=success'"
   {-- NETFILTER-CFG --}
  , "type=NETFILTER_CFG msg=audit(1576087603.167:43): table=filter family=2 entries=0"
  {-- SYSCALL --}
  , "type=SYSCALL msg=audit(1590587733.869:21): arch=c000003e syscall=1 success=yes exit=16017 a0=6 a1=565010e4e840 a2=3e91 a3=0 items=0 ppid=646 pid=656 auid=4294967295 uid=0 gid=0 euid=0 suid=0 fsuid=0 egid=0 sgid=0 fsgid=0 tty=(none) ses=4294967295 comm=\"apparmor_parser\" exe=\"/sbin/apparmor_parser\" key=(null)"
  , "type=SYSCALL msg=audit(1576087603.167:43): arch=c000003e syscall=175 success=yes exit=0 a0=2050420 a1=1db5 a2=41a94e a3=204c500 items=0 ppid=849 pid=850 auid=4294967295 uid=0 gid=0 euid=0 suid=0 fsuid=0 egid=0 sgid=0 fsgid=0 tty=(none) ses=4294967295 comm=\"modprobe\" exe=\"/usr/bin/kmod\" key=(null)"
  {-- PROCTITLE --}
  , "type=PROCTITLE msg=audit(1576087603.167:43): proctitle=2F7362696E2F6D6F6470726F6265002D71002D2D0069707461626C655F66696C746572"
  {-- SERVICE-STOP --}
  , "type=SERVICE_STOP msg=audit(1590588943.303:85): pid=1 uid=0 auid=4294967295 ses=4294967295 msg='unit=motd-news comm=\"systemd\" exe=\"/lib/systemd/systemd\" hostname=? addr=? terminal=? res=success'"
  {-- USER-START --}
  , "type=USER_START msg=audit(1590592621.270:95): pid=4672 uid=0 auid=0 ses=4 msg='op=PAM:session_open acct=\"root\" exe=\"/usr/sbin/cron\" hostname=? addr=? terminal=cron res=success'"
  {-- USER-CMD --}
  , "type=USER_CMD msg=audit(1589480776.259:38): pid=4832 uid=1000 auid=1000 ses=4 msg='cwd=\"/home/xxxx\" cmd=73797374656D63746C2073746174757320617564697464 terminal=pts/1 res=success'"
  {-- USER-END --}
  , "type=USER_END msg=audit(1590589021.193:91): pid=4302 uid=0 auid=0 ses=3 msg='op=PAM:session_close acct=\"root\" exe=\"/usr/sbin/cron\" hostname=? addr=? terminal=cron res=success'"
  {-- USER-LOGIN --}
  , "type=USER_LOGIN msg=audit(1590587826.216:81): pid=3380 uid=0 auid=1000 ses=1 msg='op=login id=1000 exe=\"/usr/sbin/sshd\" hostname=192.168.208.1 addr=192.168.208.1 terminal=/dev/pts/0 res=success'"
  {-- USER-AUTH --}
  , "type=USER_AUTH msg=audit(1576087608.595:122): pid=1643 uid=0 auid=4294967295 ses=4294967295 msg='op=PAM:authentication grantors=pam_permit acct=\"gdm\" exe=\"/usr/libexec/gdm-session-worker\" hostname=xxxx2077-vm02 addr=? terminal=/dev/tty1 res=success'"
  {-- USER-ACCT --}
  , "type=USER_ACCT msg=audit(1576087608.595:123): pid=1643 uid=0 auid=4294967295 ses=4294967295 msg='op=PAM:accounting grantors=pam_permit acct=\"gdm\" exe=\"/usr/libexec/gdm-session-worker\" hostname=xxxx2077-vm02 addr=? terminal=/dev/tty1 res=success'"
  {-- CRED-ACQ --}
  , "type=CRED_ACQ msg=audit(1576087608.598:124): pid=1643 uid=0 auid=4294967295 ses=4294967295 msg='op=PAM:setcred grantors=pam_permit acct=\"gdm\" exe=\"/usr/libexec/gdm-session-worker\" hostname=xxxx2077-vm02 addr=? terminal=/dev/tty1 res=success'"
  {-- CRED-DISP --}
  , "type=CRED_DISP msg=audit(1576087719.268:193): pid=12566 uid=0 auid=1000 ses=1 msg='op=PAM:setcred grantors=pam_unix acct=\"root\" exe=\"/usr/bin/sudo\" hostname=? addr=? terminal=/dev/pts/0 res=success'"
  {-- CRED-REFR --}
  , "type=CRED_REFR msg=audit(1576087719.252:190): pid=12566 uid=0 auid=1000 ses=1 msg='op=PAM:setcred grantors=pam_unix acct=\"root\" exe=\"/usr/bin/sudo\" hostname=? addr=? terminal=/dev/pts/0 res=success'"
  {-- ANOM-PROMISC --}
  , "type=ANOM_PROMISCUOUS msg=audit(1576087634.885:143): dev=virbr0-nic prom=256 old_prom=0 auid=4294967295 uid=0 gid=0 ses=4294967295"
  {-- LOGIN --}
  , "type=LOGIN msg=audit(1590589021.189:88): pid=4302 uid=0 old-auid=4294967295 auid=0 tty=(none) old-ses=4294967295 ses=3 res=1"
  {-- DAEMON-END --}
  , "type=DAEMON_END msg=audit(1589480838.092:9118): op=terminate auid=0 pid=1 subj=unconfined res=success"
  ]
{-- todo: see [linux/audit.h](/torvalds/linux/blob/master/include/uapi/linux/audit.h) --}
{-- todo: see [Audit Record Types](https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/6/html/security_guide/sec-audit_record_types) --}
