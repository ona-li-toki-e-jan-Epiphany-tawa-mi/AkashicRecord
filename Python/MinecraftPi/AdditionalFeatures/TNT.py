from mcpi import block

""" Makes tnt blocks that were right-clicked by players explodable. """
def light_tnt(instance, block_event, is_multiplayer):
    target_block = instance.getBlockWithData(block_event.pos)

    if target_block.id == block.TNT.id and target_block.data == 0:
        instance.setBlock(block_event.pos, block.TNT.id, 1)