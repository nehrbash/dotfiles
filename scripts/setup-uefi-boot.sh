#!/bin/sh
# Run this AFTER booting in UEFI mode (disable CSM/Legacy in BIOS first).
# Steps:
#   1. Reboot into BIOS setup (Del/F2/F12 at POST)
#   2. Disable CSM / Legacy boot
#   3. Enable UEFI boot only
#   4. Boot from the ESP on nvme0n1p1 (select EFI/Guix/grubx64.efi if prompted)
#   5. Once booted, run this script

set -e

# Verify we're in UEFI mode
if [ ! -d /sys/firmware/efi ]; then
    echo "ERROR: Not booted in UEFI mode. Configure your BIOS first."
    exit 1
fi

echo "UEFI mode confirmed."

# Reconfigure system so GRUB is properly installed as EFI
echo "Reconfiguring system (reinstalls GRUB as EFI)..."
sudo guix system reconfigure /home/nehrbash/src/dotfiles/systems/redfish.scm

# Create UEFI boot entry
echo "Creating UEFI boot entry..."
guix shell efibootmgr -- sudo efibootmgr \
    --create \
    --disk /dev/nvme0n1 \
    --part 1 \
    --loader /EFI/Guix/grubx64.efi \
    --label "Guix System" \
    --verbose

# Show result
echo ""
echo "Boot entries:"
guix shell efibootmgr -- efibootmgr -v
