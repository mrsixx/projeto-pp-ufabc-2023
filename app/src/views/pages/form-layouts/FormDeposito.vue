<script setup>
import apiService from '@/services/api-service';
import qrCode from '@images/pages/deposit-qr-code.png';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import { defineEmits } from 'vue'
import router from '@/router';
const emit = defineEmits(['showError'])
const valor = ref('0,0')
const form = ref(false)
const correntistaStore = useCorrentistaStore()

function depositar() {
  if (!form) return

  const floatValue = getFloatValue(valor.value)
  apiService.depositar(correntistaStore.contaCorrentePrincipalId, floatValue)
    .then(({data}) => {
      if(data.result === 'failure')
        emit('showError', data.error)
      else
        router.push('/dashboard')
    })
    .catch(e => emit('showError', {code: 1, message: e.message}))
}

function getFloatValue(v) {
  if(!v) return 0
  return parseFloat(v.replace(/\./g, '').replace(',', '.'))
}

function required (v) {
  return getFloatValue(v) > 0 || 'Digite um valor válido'
}
</script>

<template>
  <VForm v-model="form" @submit.prevent="depositar">
    <VRow>
      <VCol cols="12">
        <VTextField
          :rules="[required]"
          type="text"
          v-mask-decimal.br="2"
          v-model="valor"
          label="Valor"
          placeholder="0,00"
        />
      </VCol>
      <VCol cols="12" class="d-flex justify-center" v-if="form">
        <v-layout align-center justify-center>
          <VImg :src="qrCode" :width="100" aspect-ratio="2">
          </VImg>
        </v-layout>
      </VCol>
      <VCol
      cols="12"
      class="d-flex gap-4"
      >
      <div class="text-overline my-1" v-if="form">
        Escaneie o QR CODE acima e clique no botão para prosseguir com o depósito
      </div>
      </VCol>
      <VCol
      cols="12"
      class="d-flex gap-4"
      >
        <VBtn type="submit" :disabled="!form">
          Pronto, já realizei o pagamento 🚀
        </VBtn>

      </VCol>
    </VRow>
  </VForm>
</template>
