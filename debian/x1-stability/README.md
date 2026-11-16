# ThinkPad X1 Carbon Gen 11 on Debian 13

This machine is solid on Debian 12 and unstable on every newer distro. Two
separate problems turned up, with a workaround for each:

| Problem | Cause | Workaround |
|---|---|---|
| Random segfaults in unrelated programs | CPU microcode `0x6133` | `intel-microcode` held at `0x4129` |
| Sudden resets and freezes, nothing logged | Debian 13's i915 graphics firmware (suspected, under test) | Debian 12's GuC and DMC firmware in `/lib/firmware/updates` |

- Machine: ThinkPad X1 Carbon Gen 11 (21HMS02J00), BIOS N3XET63W (1.38)
- CPU: i7-1370P, Raptor Lake-P, signature `0xb06a2` (`intel-ucode/06-ba-02`)
- Graphics: Iris Xe, `i915` driver
- System: Debian 13 (trixie), kernel 6.12.107+deb13-amd64

## Symptoms

- **Segfaults:** dozens per boot in `zsh`, `nemo-desktop` (libgtk) and
  `xapp-sn-watcher` (libgio), starting minutes after boot; the kernel
  suppressed 121 in one burst. One `zsh` fault wrote to `0x40000000`, a single
  set bit: a flipped bit, not a software bug.
- **Resets:** the journal stops mid-session with no shutdown sequence and no
  kernel message before it. Nothing in pstore.
- **Freeze:** on 6.1, nothing new started (no app or tmux-pane scopes in the
  journal) while running daemons kept logging; no error, no "hung task"
  report from the kernel.

## What was tested (2026-11-15/16)

Each row is one boot. Debian 13 throughout; `i915.enable_psr=0` from the third
row on.

| Kernel | Microcode | i915 firmware | Segfaults | Failure |
|---|---|---|---|---|
| 6.12 | `0x6133` | Debian 13 | 100+ | reset after 2h20 |
| 6.12 | `0x4128` (BIOS, `dis_ucode_ldr`) | Debian 13 | none | none; shut down cleanly after 2h25, too short to tell |
| 6.12 | `0x4129` | Debian 13 | none¹ | reset after 3h35 |
| 6.1 | `0x4129` | Debian 13 | none | desktop froze after 4 min |
| 6.12 | `0x4129` | **Debian 12** | none | none so far (1h23 at last check) |

¹ One `nemo-desktop` segfault at `0x726574`, which is ASCII text ("ter")
followed as a pointer: a use-after-free in nemo, which crashes on its own.

What this shows:

- **The microcode causes the segfaults.** They stopped as soon as `0x6133` was
  gone, with nothing else changed. It does not cause the resets: those went on
  with `0x4129`.
- **The kernel alone does not cause the resets.** 6.1 was solid on Debian 12
  but froze within minutes on Debian 13, so the cause is something Debian 13
  brings to both kernels.
- **Not PSR.** `i915.enable_psr=0` prevented neither problem.
- **Not hardware, most likely.** Three kinds of failure with nothing logged
  pointed at failing hardware, but Debian 12 runs fine on this machine.
- **Graphics firmware is the lead.** Of the firmware these drivers load, the
  GuC (the GPU's scheduler) and DMC (display power) differ between Debian 12
  and 13. A GuC fault can hang the GPU without the kernel noticing, which fits
  the frozen desktop. Wi-Fi firmware differs too but was left alone: 6.12 uses
  `iwlwifi-so-a0-gf-a0-89.ucode`, which Debian 12 never had.

The last row decides it. Previous 6.12 boots failed by 3h35, so a full day
without a reset or freeze points to the firmware. If it fails, the next
suspect is the rest of Debian 13's userspace, Mesa first.

## Workaround 1: microcode pinned at 0x4129

`intel-microcode` is held at `3.20250812.1~deb12u1` from bookworm-security,
revision `0x4129`, the last of the `0x41xx` series. Debian 13 ships
`3.20251111.1~deb13u1`, revision `0x6133`.

Revisions after `0x4129` for this CPU, and what holding back costs:

- `0x6133` (20251111): functional fixes and erratum RPL070 (`REP CMPSB`/`REP
  SCASB` wrong under multi-core races). No security fixes.
- `0x6134` (20260210): INTEL-SA-01083 (CVE-2024-24853) and INTEL-SA-01396
  (CVE-2025-31648). Both need an already privileged local attacker with SMM or
  startup-code control.
- `0x6136` (20261110, sid only): functional fixes.

The kernel's mitigations are unaffected: `/sys/devices/system/cpu/vulnerabilities`
lists nothing as "Vulnerable" on `0x4129`.

Why Debian 12 never showed this: not established. Its apt sources may have
lacked `non-free-firmware`, so it never got `0x6133`.

Each microcode revision was tested for one boot only. Once the firmware
question is settled, boot `0x6133` once more with the Debian 12 firmware: if
the segfaults come back, the microcode is a second problem of its own; if not,
the firmware was behind both and the pin can go.

## Workaround 2: Debian 12 graphics firmware

`/lib/firmware/updates/i915/` holds Debian 12's `adlp_guc_70.bin` (GuC 70.5.1)
and `adlp_dmc.bin` (DMC v2.18), from `firmware-misc-nonfree 20230210-5`. The
kernel searches `/lib/firmware/updates` first, so the Debian 13 packages stay
installed. 6.12 recommends GuC 70.12.1 and warns, but loads 70.5.1 with
submission, SLPC and RC6 enabled.

Install with `install-bookworm-i915-firmware.sh` (as root). It downloads the
package, checks both files' SHA-256, installs them and rebuilds the initramfs.
Check after a reboot:

```sh
pkexec journalctl -b -k --no-pager | grep -iE 'guc|dmc'
```

It should show `adlp_dmc.bin (v2.18)` and `adlp_guc_70.bin version 70.5.1`,
with nothing failed or wedged.

## Files

| File here | Installed at | Purpose |
|---|---|---|
| `bookworm.sources` | `/etc/apt/sources.list.d/` | bookworm and bookworm-security, `non-free-firmware` on the latter for `intel-microcode` |
| `bookworm-pin` | `/etc/apt/preferences.d/` | priority 1: nothing comes from bookworm unless asked by exact version |
| `11_no_microcode` | `/etc/grub.d/` (executable) | GRUB entry "Debian GNU/Linux (no microcode update)": default kernel with `dis_ucode_ldr`, without `i915.enable_psr=0` |
| `install-bookworm-i915-firmware.sh` | run once as root | Debian 12 GuC and DMC into `/lib/firmware/updates/i915/` |

```sh
sudo cp bookworm.sources /etc/apt/sources.list.d/
sudo cp bookworm-pin /etc/apt/preferences.d/
sudo install -m 755 11_no_microcode /etc/grub.d/ && sudo update-grub
sudo apt update
sudo apt install --allow-downgrades intel-microcode=3.20250812.1~deb12u1
sudo apt-mark hold intel-microcode
sudo ./install-bookworm-i915-firmware.sh
```

Check after a reboot: `grep -m1 microcode /proc/cpuinfo` shows `0x4129`.

Also on the system, from testing:

- `linux-image-6.1.0-55-amd64` from bookworm-security, under "Advanced
  options" in GRUB. Not needed if 6.12 holds; remove with
  `sudo apt purge linux-image-6.1.0-55-amd64`.
- `GRUB_CMDLINE_LINUX="i915.enable_psr=0"` in `/etc/default/grub`. It
  prevents neither problem; harmless, can go.

## Debugging the next failure

- **Segfaults:** `pkexec journalctl -k -b -g 'segfault|general protection' --no-pager | grep -v nemo-desktop`
  fills up within an hour or two on a bad setup.
- **Freeze:** enable all SysRq keys first with
  `pkexec /usr/sbin/sysctl kernel.sysrq=1` (the default, 438, lacks the debug
  dumps). Then try Ctrl+Alt+F3 for a text console; failing that,
  Alt+PrtSc+W dumps blocked processes to the kernel log, and Alt+PrtSc+S, U, B
  syncs and reboots.
- **Reset:** nothing to catch live; read the previous boot with
  `pkexec journalctl -b -1 -k -n 50 --no-pager` and check whether the
  journal ends without "Stopping…" lines.

`pkexec` takes programs from the user's `PATH`, so give `/usr/sbin` tools by
full path.

## Moving on

- **Microcode:** `0x6134` (`3.20260925.1~deb13u1`) is in
  trixie-proposed-updates and arrives with the next point release. To test:
  `sudo apt-mark unhold intel-microcode`, install it, use the machine for a
  day, check for segfaults. To go back:
  `sudo apt install --allow-downgrades intel-microcode=3.20250812.1~deb12u1 && sudo apt-mark hold intel-microcode`.
- **Firmware:** if Debian 12's firmware turns out to be the fix, report it
  against `firmware-nonfree` in Debian and to the i915 tracker
  (gitlab.freedesktop.org/drm/i915/kernel), giving the GuC and DMC versions on
  both sides. Retest newer firmware when `firmware-nonfree` updates in trixie.
- **BIOS:** stay on 1.38 until Lenovo ships a fixed microcode revision; a BIOS
  update can carry `0x6133` in the firmware itself, which `dis_ucode_ldr`
  cannot undo.

To remove everything: unhold `intel-microcode`, delete the three files from
`/etc` and `/lib/firmware/updates/i915`, then
`sudo update-initramfs -u -k all && sudo update-grub && sudo apt update`.
