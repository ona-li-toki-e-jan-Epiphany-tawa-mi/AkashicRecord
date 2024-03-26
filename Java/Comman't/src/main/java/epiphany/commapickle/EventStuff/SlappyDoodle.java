package epiphany.commapickle.EventStuff;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraft.util.EnumHand;
import net.minecraft.util.text.TextComponentString;
import net.minecraftforge.event.entity.living.LivingKnockBackEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

public class SlappyDoodle {

    @SubscribeEvent
    public void onSlap(LivingKnockBackEvent e) {
        Entity enny = e.getAttacker();

        if (enny instanceof EntityLivingBase) {
            ItemStack hitter = ((EntityLivingBase) enny).getHeldItem(EnumHand.MAIN_HAND);

            if (hitter.getItem() == Items.FISH)
                if (enny instanceof EntityPlayer)
                    enny.sendMessage(new TextComponentString("WORRKK"));
        }
    }
}
