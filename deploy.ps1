# Because plots are uniquely determined by some hash by pandoc-plot,
# images can build up on the remote server
# We clean out all files before transfer
echo "Delete current website (skip with CTRL-C):"
ssh decotret@gollum.physics.mcgill.ca 'rm -r -f /common/WWW/decotret/*';

echo "Transfer last-recently built website:"
scp -Cr ./decotret decotret@gollum.physics.mcgill.ca:/common/WWW/