# Because plots are uniquely determined by some hash by pandoc-plot,
# images can build up on the remote server
# We clean out all files before transfer
ssh decotret@gollum.physics.mcgill.ca 'rm -r -f /common/WWW/decotret/*';

scp -Cr ./decotret decotret@gollum.physics.mcgill.ca:/common/WWW/