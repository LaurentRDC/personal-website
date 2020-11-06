# Because plots are uniquely determined by some hash by pandoc-plot,
# images can build up on the remote server
# We clean out all files before transfer
# Note that after all files are deleted, the siwicklab website (from ~/website) needs
# to be re-transferred
echo "Delete current website (skip with CTRL-C):"
ssh decotret@gollum.physics.mcgill.ca "rm -r -f /common/WWW/decotret/*; rsync -a 'website/' /WWW/decotret/siwicklab";

echo "Transfer last-recently built website:"
scp -Cr ./decotret decotret@gollum.physics.mcgill.ca:/common/WWW/