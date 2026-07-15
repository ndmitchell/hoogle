{ hoogle, cores ? 4 }:
{ lib, pkgs, ... }:

# The Plan:
# Hoogle serves on a uniquely-named UNIX domain socket which we
# configure nginx to forward to via the nginxConf configuration
# fragment. The Hoogle database is periodically updated by
# rotate-hoogle.service which generates a new Hoogle database,
# starts a new hoogle server, updates the nginx configuration
# and shuts down the old server.

let
  hoogleRun = "/run/hoogle";
  nginxConf = "${hoogleRun}/nginx.conf";
  socket = inst: "/run/hoogle/hoogle-${inst}.sock";
  nServers = 12;
in
{
  users.users.hoogle = {
    isSystemUser = true;
    description = "hoogle server";
    group = "hoogle";
  };

  users.groups.hoogle = {
    members = [ "nginx" ];
  };

  systemd.timers."generate-hoogle" = {
    description = "Rotate Hoogle instance";
    timerConfig = {
      Unit = "generate-hoogle.service";
      OnCalendar = "hourly";
      Persistent = true;
    };
    wantedBy = [ "timers.target" ];
  };

  systemd.services = {
    "generate-hoogle" = {
      script = ''
        hoogle generate --database=$DB_DIR/haskell-new.hoo --insecure --download +RTS -N${toString cores} -RTS
        mv $DB_DIR/haskell-new.hoo $DB_DIR/haskell.hoo
      '';
      path = [ hoogle ];
      after = [ "network.target" ];
      serviceConfig = {
        User = "hoogle";
        Group = "hoogle";
        Type = "oneshot";
        TimeoutStartSec = 600;
        ExecStartPost = "+systemctl restart 'hoogle@*'";
        BindReadOnlyPaths = [
          # mount the nix store read-only
          "/nix/store"
          # getAppUserDataDirectory needs getUserEntryForID
          "/etc/passwd"
        ];
        PrivateNetwork = false;
        RuntimeDirectory = "generate-hoogle";
        StateDirectory = [ "hoogle" ];
      };
      environment = {
        DB_DIR = "%S/hoogle";
      };
    };

    "hoogle@" = {
      script = ''
        ${hoogle}/bin/hoogle serve \
          --database=$DB_DIR/haskell.hoo \
          --scope=set:stackage \
          --socket=$SOCKET \
          --links \
          +RTS -T -RTS;
      '';
      serviceConfig = {
        User = "nginx";
        Group = "hoogle";
        BindReadOnlyPaths = [
          # mount the nix store read-only
          "/nix/store"
          # getAppUserDataDirectory needs getUserEntryForID
          "/etc/passwd"
          #"/etc/ssl" "/etc/ssl/certs"
        ];
        PrivateTmp = false;
        ProtectSystem = false;
        ProtectHome = false;
        NoNewPrivileges = false;
        Restart = "on-failure";
        RestartSec = "5s";
        RuntimeDirectory = "hoogle";
      };
      environment = {
        DB_DIR = "%S/hoogle";
        SOCKET = socket "%i";
        SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      };
    };
  } // (
    let
      mkInst = n:
        lib.attrsets.nameValuePair "hoogle@${toString n}"
        {
          wantedBy = [ "multi-user.target" ];
          overrideStrategy = "asDropin";
        };
    in lib.listToAttrs (map mkInst (lib.range 1 nServers))
  );

  systemd.tmpfiles.rules = [
    "f ${nginxConf} 0755 nginx nginx -"
  ];

  services.nginx.upstreams.hoogle.servers =
    let mkInst = n: lib.attrsets.nameValuePair "unix:${socket (toString n)}" { };
    in lib.listToAttrs (map mkInst (lib.range 1 nServers));
}

